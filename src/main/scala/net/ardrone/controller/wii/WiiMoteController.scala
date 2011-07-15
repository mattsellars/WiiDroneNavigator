package net.ardrone.controller.wii

import motej.event.{CoreButtonListener, CoreButtonEvent, ExtensionListener, ExtensionEvent, MoteDisconnectedEvent}
import motej.{Mote, MoteFinder, MoteFinderListener}
import motejx.extensions.nunchuk.{Nunchuk, AnalogStickEvent,AnalogStickListener}
import se.scalablesolutions.akka.actor.ActorRef
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.actor.Actor._
import se.scalablesolutions.akka.actor.ActorRegistry
import collection.mutable.{Set, Map, MutableList}
import motej.request.ReportModeRequest;
import org.apache.log4j.Logger;
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import net.ardrone.model.DroneActorMoveMentCommands._
import net.ardrone.model.DroneSystemCommands
import net.ardrone.model.DroneCommands
import net.ardrone.model.WiiMoteCommands
import net.ardrone.model.TerminalCommands._
import net.ardrone.actor.util.ActorUtils._
import net.ardrone.concurrent.NamedThreadPoolFactory
import java.util.concurrent.ConcurrentHashMap
import java.util.{Map => JMap}
import motejx.extensions.nunchuk.NunchukButtonListener
import motejx.extensions.nunchuk.NunchukButtonEvent

/**
 * WiiMote communications actor 
 * @author Matt Sellars
 *
 */
class WiiMoteActor extends Actor {
    import WiiMoteController._
    import ListenerMode._
    
    val registerCommand = """register wiimote( [-a-zA-Z0-9]+)?""".r
    val regAckCommand = """regack ([-a-zA-Z0-9]+)""".r
    
    def receive = {
        case WiiMoteCommands.StartBlueTooth => { 
            initBlueToothForWiiMoteConnections
            self.channel ! true
        }
        case registerCommand(name) => {
            WiiMoteController.lookupWiiMote(name.trim)
        }
        case regAckCommand(name) => {
            getTerminalActorRef ! "WIIMOTE "+name+" REGISTERED"
            attachDroneActorToListener(name)
        }
        case WiiMoteCommands.RumbleWiiMote(rumbleTimes, rumbleTimeMillis) => {
            WiiMoteController.rumble(rumbleTimes, rumbleTimeMillis)
        }
        case WiiMoteCommands.EnterDualChukMode => {
            val moteCount = WiiMoteController.getMoteCount
            if (moteCount == 2) {
                WiiMoteController.log.info("Entering dual-chuk mode")
                val orderedMotes = WiiMoteController.getMotesInOrderOfRegistration
                val mote1 = orderedMotes(0)
                val mote2 = orderedMotes(1)
                val angleListener = angleChuckControlListener
                WiiMoteController.synchDroneAndWiiMote(mote1, angleListener, angleListener, angleListener)
                
                val vertListener = virtTurnChuckControlListener
                WiiMoteController.synchDroneAndWiiMote(mote2, vertListener, vertListener, vertListener)
                WiiMoteController.setCurrentControlMode(WiiControlMode.DUAL_CHUK)
                
                var moteNames = new StringBuilder
                var moteCount = 0
                for (moteHolder <- WiiMoteController.getMotesInOrderOfRegistration) {
                    if (moteCount > 0) {
                        moteNames.append(", ")
                    }
                    moteNames.append(moteHolder.mote.getBluetoothAddress)
                }
                getTerminalActorRef ! ControlModeUpdateAcknowedgeCommand(WiiControlMode.DUAL_CHUK, moteNames.toString)
            }
        }
        case WiiMoteCommands.EnterSingleChukMode => {
            log.info("Before Single chuck mode")
            val moteHolder = WiiMoteController.getFirstMoteHolder
            if (null != moteHolder) {
                WiiMoteController.log.info("Entering single-chuk mode")
                var controlSync:SingleMoteChuckDroneControlListener = WiiMoteController.singleMoteChukControlListener
                WiiMoteController.synchDroneAndWiiMote(moteHolder, controlSync, controlSync, null)
                WiiMoteController.setCurrentControlMode(WiiControlMode.SINGLE_CHUK)
                getTerminalActorRef ! ControlModeUpdateAcknowedgeCommand(WiiControlMode.SINGLE_CHUK, 
                                        WiiMoteController.getFirstMoteHolder.mote.getBluetoothAddress)
            } else {
                log.error("Existing holder is null")
            }
        }
        case WiiMoteCommands.GetCurrentControlMode => {
            self.channel ! WiiMoteController.getCurrentControlMode
        }
        case WiiMoteCommands.GetWiiMoteCount => {
            val count = WiiMoteController.getMoteCount
            self.channel ! count
        }
        case "QUIT" => {
            WiiMoteController.shutdown
        }
        case x:Any => {
            WiiMoteController.log.info("Unknown WiiActor Command: "+x)
        }
    }
    
    def attachDroneActorToListener(name:String) = {
        if (getMoteCount == 1) {
            self ! WiiMoteCommands.EnterSingleChukMode
        } else {
            self ! WiiMoteCommands.EnterDualChukMode
        }
    }   
}

/**
 * WiiMote+Nunchuk control modes
 * @author Matt Sellars
 *
 */
object WiiControlMode extends Enumeration {
    type WiiControlMode = Value
    val SINGLE_CHUK, DUAL_CHUK = Value
}

/**
 * ListenerModes are used for dual nunchuk mode when flying with 2 joysticks
 * HORIZONTAL_AXIS mode is for forward backward movement control
 * VIRTICAL_AXIS mode is for up/down movement AND rotate left/right movement control
 * @author matt
 */
private[wii] object ListenerMode extends Enumeration {
    type ListenerMode = Value
    val HORIZONTAL_AXIS, VIRTICAL_AXIS = Value
}

/**
 * Drone controls used for dual Nunchuk of drone 
 * @author Matt Sellars
 *
 */
private[wii] class DualJoyStickDroneControlListener(val mode:ListenerMode.ListenerMode) 
        extends CoreButtonListener with AnalogStickListener with NunchukButtonListener {
    import java.awt.Point
    import ListenerMode._
    
    private val lastPoint = new AtomicReference[Point](null)
    
    // wiimote button
    def buttonPressed(evt: CoreButtonEvent) {
        if (evt.isButtonHomePressed) {
            getDroneActorRef ! DroneSystemCommands.RecoverFromEmergencyMode
        }
    } //ignored for now
    
    // nunchuck button listener
    def buttonPressed(evt :NunchukButtonEvent) {
        if (evt.isButtonCPressed && evt.isButtonZPressed) {
            mode match {
                case HORIZONTAL_AXIS => {
                    getDroneActorRef ! Land
                }
                case VIRTICAL_AXIS => {
                    getDroneActorRef ! TakeOff
                }
                case _ => logModeError _
            }
        } else {
            if (evt.isButtonZPressed) {
                mode match {
                    case HORIZONTAL_AXIS => {
                        getDroneActorRef ! GoDown
                    }
                    case VIRTICAL_AXIS => {
                        //getDroneActorRef ! TakeOff
                        getDroneActorRef ! GoUp
                    }
                    case _ => logModeError _
                }
            }
            if (evt.isButtonCPressed) {
                mode match {
                    case HORIZONTAL_AXIS => {
                        //getDroneActorRef ! TakeOff
                    }
                    case VIRTICAL_AXIS => {
                        //getDroneActorRef ! TakeOff
                        //getDroneActorRef ! GoUp
                        getDroneActorRef ! FlatTrim
                    }
                    case _ => logModeError _
                }
            }
            if (!evt.isButtonCPressed && !evt.isButtonZPressed) {
                mode match {
                    case HORIZONTAL_AXIS => {
                        //do nothing
                    }
                    case VIRTICAL_AXIS => {
                        getDroneActorRef ! SetGaz(0)
                    }
                    case _ => logModeError _
                }
            }
        }
    }
    
    def analogStickChanged(evt :AnalogStickEvent) {
        val p = evt.getPoint();
        if (lastPoint.get == null){
            lastPoint.set(p);
        }
        val stickForwardsBackwardsMovement:Float = -(lastPoint.get.getX()-p.getX()).asInstanceOf[Float]/100
        val stickSideToSideMovement:Float = (lastPoint.get.getY()-p.getY()).asInstanceOf[Float]/100
        mode match {
            case HORIZONTAL_AXIS => {
                //getDroneActorRef ! SetPitch(stickForwardsBackwardsMovement) // forward/backwards lean
                //getDroneActorRef ! SetRoll(stickSideToSideMovement) // left/right lean
                getDroneActorRef ! SetPitchAndRoll(stickForwardsBackwardsMovement, stickSideToSideMovement);
            }
            case VIRTICAL_AXIS => {
                //getDroneActorRef ! SetGaz(-stickSideToSideMovement) // up/down (gaz)
                getDroneActorRef ! SetYaw(stickForwardsBackwardsMovement)  // left/right (yaw)
            }
            case _ => logModeError _
        }
    }
    
    private def logModeError(mode:Any) {
        WiiMoteController.log.error("DualJoyStickDroneControlListener in unknown mode: "+mode)
    }
    
    override def toString :String = {
        def renderMode :String = {
            if (mode == HORIZONTAL_AXIS) {
                "mode:HORIZONTAL_AXIS"
            } else {
                "mode:VIRTICAL_AXIS"
            }
        }
        "[DualJoyStickDroneControlListener " + renderMode + "]"
    }
}

/**
 * Drone controls used for single WiiMote + Nunchuk
 * @author Matt Sellars
 *
 */
private[wii] class SingleMoteChuckDroneControlListener extends CoreButtonListener with AnalogStickListener {
    import java.awt.Point
    
    //keep initial point for euler angle calculation range [-1, 1] in x,y plane
    private val initialPoint = new AtomicReference[Point](null)
    private var lastEventNoButtonPressed = true;
    
    def buttonPressed(evt: CoreButtonEvent) {
        if (evt.isNoButtonPressed) {
            getDroneActorRef ! StopMovement
        }
        if (evt.isButtonAPressed) {
            getDroneActorRef ! DroneSystemCommands.ResetConnection
        }
        if (evt.isButtonHomePressed) { //recover
            getDroneActorRef ! DroneSystemCommands.RecoverFromEmergencyMode
        }
        if (evt.isButtonTwoPressed) {
            getDroneActorRef ! Land
        }
        if (evt.isButtonOnePressed) {
            getDroneActorRef ! TakeOff
        }
        if (evt.isDPadUpPressed) {
            getDroneActorRef ! GoUp
        } else if (evt.isDPadDownPressed) {
            getDroneActorRef ! GoDown
        }
        if (lastEventNoButtonPressed) {
            if (evt.isDPadLeftPressed) {
                getDroneActorRef ! TurnLeft
            } else if (evt.isDPadRightPressed) {
                getDroneActorRef ! TurnRight
            }
        }
        lastEventNoButtonPressed = evt.isNoButtonPressed
    }
    
    def analogStickChanged(evt :AnalogStickEvent) {
        val p = evt.getPoint();
        if (initialPoint.get == null){
            initialPoint.set(p);
        }
        val pitch :Float = -(initialPoint.get.getX()-p.getX()).asInstanceOf[Float]/100
        val roll :Float = (initialPoint.get.getY()-p.getY()).asInstanceOf[Float]/100
        getDroneActorRef ! SetPitch(pitch)
        getDroneActorRef ! SetRoll(roll)
    }
    
    override def toString :String = {
        "[SingleWiiMoteControlListener]"
    }
}

/**
 * WiiMote and Nunchuk combind listener
 * @author Matt Sellars
 *
 */
trait ConfigurableMoteChuckListener extends CoreButtonListener with AnalogStickListener with NunchukButtonListener {
    protected var buttonListener = new AtomicReference[CoreButtonListener](null)
    protected var joystickListener =  new AtomicReference[AnalogStickListener](null)
    protected var nunchukButtonListener =  new AtomicReference[NunchukButtonListener](null)
    
    def setButtonListener(listener:CoreButtonListener) {
        WiiMoteController.log.info("Adding CoreButtonListener: "+listener)
        this.buttonListener.set(listener)
    }
    def removeCurrentButtonListener = {
        this.buttonListener.set(null)
    }
    
    def setJoystickListener(listener:AnalogStickListener) {
        WiiMoteController.log.info("Adding AnalogStickListener: "+listener)
        this.joystickListener.set(listener)
    }
    def removeCurrentJoystickListener = {
        this.joystickListener.set(null)
    }
    
    def setNunckukButtonListener(listener:NunchukButtonListener) {
        WiiMoteController.log.info("Adding NunchukButtonListener: "+listener)
        this.nunchukButtonListener.set(listener)
    }
    def removeNunchukButtonListener = {
        this.nunchukButtonListener.set(null)
    }
}
/**
 * Simple WiiMote and Nunchuk listener decorator that we use to attach a
 * listener to the WiiMote and Nunchuk and swap out the delegate at a later point
 * @author Matt Sellars
 *
 */
protected[wii] class ConfigurableMoteChuckListenerImpl extends ConfigurableMoteChuckListener {
    def buttonPressed(evt :CoreButtonEvent) {
        if (null != buttonListener.get) {
            buttonListener.get.buttonPressed(evt)
        }
    }
    def analogStickChanged(evt :AnalogStickEvent) {
        if (null != joystickListener.get) {
            joystickListener.get.analogStickChanged(evt)
        }
    }
    def buttonPressed(evt :NunchukButtonEvent) {
        if (null != nunchukButtonListener.get) {
          nunchukButtonListener.get.buttonPressed(evt)   
        }
    }
}

/**
 * Singleton WiiMoteController access
 * @author matt
 *
 */
object WiiMoteController {
    import WiiControlMode._
    import ListenerMode._
    
    val log = Logger.getLogger(WiiMoteController.getClass)
    val wiiMoteRegistryEventListener = new AtomicReference[MoteRegistryEventListener](null)
    
    /*
     * Singleton Listeners
     */
    private[wii] val singleMoteChukControlListener = new SingleMoteChuckDroneControlListener()
    private[wii] val angleChuckControlListener = new DualJoyStickDroneControlListener(HORIZONTAL_AXIS)
    private[wii] val virtTurnChuckControlListener = new DualJoyStickDroneControlListener(VIRTICAL_AXIS)
   
    val MOTE_INDEX_LOCK = new Object
    val nameToWiiMote = new ConcurrentHashMap[String, MoteHolder]()
   
    val currentControlMode = new AtomicReference[WiiControlMode](WiiControlMode.SINGLE_CHUK)
   
    protected[wii] def getCurrentControlMode = currentControlMode.get
    protected[wii] def setCurrentControlMode(mode:WiiControlMode) = currentControlMode.set(mode)
    protected[wii] def getMoteCount = nameToWiiMote.size
   
    protected[wii] def rumble(rumbleCount:Int, rumbleTimeMillis:Long) :Unit = {
        val orderedMotes = WiiMoteController.getMotesInOrderOfRegistration
        for(i <- 0 until rumbleCount) {
            for(j <- 0 until orderedMotes.size ) {
                if (log.isDebugEnabled) {
                    log.debug("Rumbling wiimote: "+orderedMotes(j).mote.getBluetoothAddress)
                }
                orderedMotes(j).mote.rumble(rumbleTimeMillis)
            }
            Thread.sleep(500L)
        }
    }
    
    protected[wii] def shutdown :Unit = {
        log.info("Disconnecting any connected WiiMotes")
        var holderIter = nameToWiiMote.values.iterator
        while (holderIter.hasNext) {
            val holder = holderIter.next
            if (holder.mote != null) {
                try {
                    holder.mote.disconnect()
                } catch {
                    case t:Throwable => log.error("Error disconnection WiiMote", t)
                }
            }
        }
    }
    
    protected[wii] def getFirstMoteHolder :MoteHolder = {
       val holderEntries = nameToWiiMote.entrySet.iterator
       while (holderEntries.hasNext) {
           var holderEntry = holderEntries.next
           if (holderEntry.getValue.order == 0) {
               return holderEntry.getValue
           } else if (!holderEntries.hasNext) {
               holderEntry.setValue(new MoteHolder(0, holderEntry.getValue.mote, holderEntry.getValue.configurableListener))
               return holderEntry.getValue
           }
       }
       //shouldn't get here... if we do return what you got :-(
       return null
    }
    
    protected[wii] def initBlueToothForWiiMoteConnections {
        if (wiiMoteRegistryEventListener.get == null) {
           wiiMoteRegistryEventListener.set(new MoteRegistryEventListener(nameToWiiMote))
       }
        WiiMoteRegistry.addRegistryListener(wiiMoteRegistryEventListener.get)
    }
    
    protected[wii] def lookupWiiMote(name:String) {
       if (!nameToWiiMote.contains(name)) {
           wiiMoteRegistryEventListener.get.findWiiMote(name)
       } else {
           getTerminalActorRef ! new PrintStatus("WiiMote already registered for name: "+name+".  Please choose another name.")
       }
    }

    protected[wii] def synchDroneAndWiiMote(name :String, 
                                            buttonListener :CoreButtonListener,
                                            stickListener :AnalogStickListener,
                                            chukButtonListener :NunchukButtonListener) {
       val holder = nameToWiiMote.get(name)
       synchDroneAndWiiMote(holder, buttonListener, stickListener, chukButtonListener)
    }
   
    protected[wii] def synchDroneAndWiiMote(holder :MoteHolder, 
                                            buttonListener :CoreButtonListener,
                                            stickListener :AnalogStickListener,
                                            chukButtonListener :NunchukButtonListener) {
       WiiMoteController.log.info("synchDroneAndWiiMote called for mote: " + holder.mote.getBluetoothAddress)
       if (null != buttonListener) {
           holder.configurableListener.setButtonListener(buttonListener)
       } else {
           WiiMoteController.log.warn("CoreButtonListener is null")
       }
       if (null != stickListener) {
           holder.configurableListener.setJoystickListener(stickListener)
       } else {
           WiiMoteController.log.warn("AnalogStickListener is null")
       }
       if (null != chukButtonListener) { 
           holder.configurableListener.setNunckukButtonListener(chukButtonListener)
       } else {
           WiiMoteController.log.warn("NunchukButtonListener is null")
       }
    }
    
    protected[wii] def disconnectDroneAndWiiMotes() {
        var holderIter = nameToWiiMote.values.iterator
        while(holderIter.hasNext) {
            val holder = holderIter.next
            holder.configurableListener.setButtonListener(null)
            holder.configurableListener.setJoystickListener(null)
            holder.configurableListener.setNunckukButtonListener(null)
        }
        nameToWiiMote.clear
    }
   
    protected[wii] def getMotesInOrderOfRegistration :List[MoteHolder] = {
       val holders = nameToWiiMote.values
       if (holders.size == 2) {
           val hIter = holders.iterator
           val holder1 = hIter.next
           val holder2 = hIter.next
           
           if (holder1.order > holder2.order) {
               return List(holder2, holder1)
           } else {
               return List(holder1, holder2)
           }
       } else {
           return List(holders.iterator.next)
       }
    }
   
    protected def setMoteLeds(mote:Mote, ledsOn:Int) :Unit = {
        mote.setPlayerLeds(Array((ledsOn >= 1), (ledsOn >= 2), (ledsOn >= 3), (ledsOn >= 4)));
    }
   
    protected class MoteHolder(val order:Int, val mote:Mote, val configurableListener:ConfigurableMoteChuckListener)
   
    protected class MoteRegistryEventListener(nameToWiiMote :JMap[String, MoteHolder]) extends 
            WiiMoteRegistryListener with ExtensionListener {
       
       val log :Logger = Logger.getLogger(WiiMoteController.getClass)
       val searcher = new MoteSearcherThread
       
       def findWiiMote(name:String) {
           getTerminalActorRef ! new PrintStatusProgress("""
Make sure the Nunchuk is connected to the WiiMote then periodically
press and hold buttons 1 and 2 on the WiiMote while I search for it.
If you don't see acknowledgement of the added WiiMote then try again.

Searching - Usually 20 seconds depending on how many bluetooth devices are around...""")
           searcher.startSearch()
       }
       
       def moteFound(mote:Mote) {
           try {
               val message = "WiiMote found at address: "+mote.getBluetoothAddress
               WiiMoteController.log.info(message)
               getTerminalActorRef ! new PrintStatus(message)
               val moteHolder = new MoteHolder(nameToWiiMote.size, mote, new ConfigurableMoteChuckListenerImpl())
               setMoteLeds(mote, nameToWiiMote.size + 1)
               mote.addExtensionListener(this)
               mote.addCoreButtonListener(moteHolder.configurableListener)
               MOTE_INDEX_LOCK.synchronized {
                   nameToWiiMote.put(mote.getBluetoothAddress, moteHolder)
               }
               
               getWiiActorRef ! "regack "+mote.getBluetoothAddress
           } catch {
               case t:Throwable => log.error("Error adding wiimote", t)
           }
           searcher.stopSearch
       }
       
       def moteDisconnected(evt:MoteDisconnectedEvent[Mote]) :Unit = {
           val mote = evt.getSource
           if (mote != null) {
               MOTE_INDEX_LOCK.synchronized {
                   try {
                       val name = mote.getBluetoothAddress
                       if (null != name) {
                           //moteAddressToName.remove(mote.getBluetoothAddress)
                           val holder = nameToWiiMote.get(name)
                           if (null != holder) nameToWiiMote.remove(name)
                           log.info("WiiMote "+name+" disconnected")
                       } else {
                           val moteHolderIter = nameToWiiMote.entrySet.iterator
                           var nameEntry :String = null
                           while(moteHolderIter.hasNext) {
                               val holderEntry = moteHolderIter.next
                               if (mote.getBluetoothAddress == holderEntry.getValue.mote.getBluetoothAddress) {
                                   nameEntry = holderEntry.getKey
                               }
                           }
                           if (nameEntry != null) {
                               nameToWiiMote.remove(nameEntry)
                           }
                       }
                           
                       log.info("Remaining Motes: "+nameToWiiMote.size)
                       if (nameToWiiMote.size == 1) {
                           getWiiActorRef !!! WiiMoteCommands.EnterSingleChukMode
                           try {
                               setMoteLeds(nameToWiiMote.values.iterator.next.mote, 1)
                           } catch {
                               case t:Throwable => { log.error(t)}
                           }
                       } else if (nameToWiiMote.size == 0) {
                           disconnectDroneAndWiiMotes
                           val msg = "No WiiMotes connected. If Drone is still flying type `land` to land drone"
                           getTerminalActorRef ! new PrintStatus(msg)
                       }
                   } catch {
                       case err:Throwable => { log.error("Error Trying to eject mote: "+mote.getBluetoothAddress, err) }
                   }
               }
           }
       }
       
       def extensionConnected(evt :ExtensionEvent) {
           WiiMoteController.log.info("Extension event recieved: "+evt.toString)
           if (null != evt.getExtension() && 
               evt.getExtension().isInstanceOf[Nunchuk]) {
               val mote:Mote = evt.getSource
               mote.setReportMode(ReportModeRequest.DATA_REPORT_0x32);
               val chuk = evt.getExtension().asInstanceOf[Nunchuk]
               //val moteName = moteAddressToName.get(mote.getBluetoothAddress)
               val holder = nameToWiiMote.get(mote.getBluetoothAddress)
               chuk.addAnalogStickListener(holder.configurableListener)
               chuk.addNunchukButtonListener(holder.configurableListener)
               WiiMoteController.log.info("Added Nunchuk for mote: "+mote.getBluetoothAddress)
           }
       }
       
       def extensionDisconnected(evt :ExtensionEvent) {
           if (null != evt.getExtension() && 
               evt.getExtension().isInstanceOf[Nunchuk]) {
               val mote:Mote = evt.getSource
               WiiMoteController.log.info("Removed Nunchuk for mote: "+mote.getBluetoothAddress)
           }
       }
       
       protected class MoteSearcherThread {
           val executorService = Executors.newFixedThreadPool(2, new NamedThreadPoolFactory("MoteSearcher"))
           
           private val runnerMutex = new Object
           private var searchRunner:Runnable = new Runnable {
                       def run() :Unit = {
                           searchCancellerRunning.set(true)
                           log.info("Starting WiiMote Search Search")
                           WiiMoteRegistry.searchForMote
                       }
                   }
           private var searchStopper:Runnable = new Runnable {
                       def run() :Unit = {
                           log.info("Stopping WiiMote Search Search")
                           WiiMoteRegistry.stopMoteSearch()
                       }
                   }
           private val searchCancellerRunning = new AtomicBoolean(false)
           private val searchCanceller = new Runnable() {
              override def run() :Unit = {
                   Thread.sleep(60000L) //1 minute
                   if (searchCancellerRunning.get) {
                       searchCancellerRunning.set(false)
                       searchStopper.run()
                   }
               }
           }
           
           def startSearch() :Unit = {
               if (!searchCancellerRunning.get) {
                   executorService.submit(searchRunner)
                   executorService.submit(searchCanceller)
               }
           }
           
           def stopSearch() :Unit = {
               searchCancellerRunning.set(false)
               executorService.submit(searchStopper)
           }
       }
    }
}