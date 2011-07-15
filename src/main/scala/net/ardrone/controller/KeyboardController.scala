package net.ardrone.controller

import se.scalablesolutions.akka.dispatch.Future
import se.scalablesolutions.akka.actor.ActorRef
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.actor.Actor._
import org.apache.log4j.{Logger, Level};
import net.ardrone.model.DroneActorMoveMentCommands.{TakeOff, Land, StopMovement, TurnLeft, TurnRight}
import net.ardrone.model.DroneSystemCommands.{ResetConnection, RecoverFromEmergencyMode}
import net.ardrone.model.DroneQueryCommands.{IsFlying, IsConnected}
import net.ardrone.model.TerminalCommands._
import net.ardrone.model.WiiMoteCommands
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import net.ardrone.controller.wii.WiiControlMode._
import net.ardrone.actor.util.ActorUtils._
/**
 *@deprecated use Terminal
 */
class KeyboardActor extends Actor {
    def receive = {
        case InitializeCommand(droneActor, wiiMoteActor) => {
            KeyboardController.initialize(droneActor, wiiMoteActor)
            self ! "ACK"
            println("Scala Drone Control Started")
        }
        case ControlModeUpdateAcknowedgeCommand(curMode, activeControls) => {
            curMode match {
                case SINGLE_CHUK => {
                    println("You are now in single WiiMote+Nunchuk mode using controller: "+activeControls.head)
                }
                case DUAL_CHUK => {
                    def printControllers {
                        var controller = ""
                        for(name <- activeControls) {
                            if (controller != "") {
                                controller += ", "
                            }
                            controller += name
                        }
                        controller
                    }
                    println("You are now in dual WiiMote+Nunchuk mode using controllers: "+printControllers)
                }
            }
            self ! "ACK"
        }
        case cmd:String => {
            if (null != cmd) KeyboardController.processCommand(cmd)
        }
    }
}

private[controller] class KeyboardController(val droneActor :ActorRef, val wiiMoteActor :ActorRef) extends Runnable {
    val running = new AtomicBoolean(true)
    val t = new Thread(this)
    t.setName("ARDroneKeyboardInput");
    t.start();
    
    def run :Unit = {
        try {
            while(running.get) {
                processCommand(readLine())
            }
        } catch {
            case t: Throwable => {
                KeyboardController.log.error("Error processing command", t)
                run
            }
        }
    }
    
    def processCommand(cmd:String) {
        cmd.toUpperCase match {
                
            case loglevelCommand(lvl) => {
                var level:Level = null
                lvl match {
                    case "DEBUG" => level = Level.DEBUG
                    case "INFO" => level = Level.INFO
                    case "WARN" => level = Level.WARN
                    case "ERROR" => level = Level.ERROR
                    case _ => {} //ignored for now
                }
                if (null != level) {
                    Logger.getRootLogger.setLevel(level)
                }
                prompt
            }
            case turnCommand(direction) => { 
                turnDirectionCommand(direction) 
                prompt
            }
            case goCommand(direction) => {
                goDirectionCommand(direction)
                prompt
            }
            case "HELP" => {
                printHelp
                prompt
            }
            case "QUIT" => {
                shutdownApplication
                prompt
            }
            case regWiiMoteCommand(name) => {
                val count = getWiiMoteCount
                if (count < 2) {
                    getDroneActorRef ! Land
                    def getName :String = {
                        if (name != null) { 
                           return name.trim
                        } else {
                           return "MOTE-"+count
                        }
                    }
                    getWiiActorRef ! "register wiimote "+getName
                } else {
                    println("***************************************")
                    println("* 2 is the maximum number of WiiMotes *")
                    println("*  You can have too many WiiMotes :)  *")
                    println("***************************************\n")
                }
            }
            case regWiiAckCommand(name) => {
                val count = getWiiMoteCount
                if (count == 1) {
                    println("\nRegistered WiiMote "+name+" - Ready To fly!\n")
                    println("You can add one more WiiMote and fly in dual-chuk")
                    println("mode or just take off now by pressing 1.  If you")
                    println("add another WiiMote you can still toggle between")
                    println("dual-chuk mode and single-chuk mode.\n")
                } else if (count == 2) {
                    println("\nRegistered second WiiMote "+name+", type `switch mode`")
                    println("to switch between single and dual WiiMote+Nunchuk control.")
//                    def enterDualChuckMode :Boolean = {
//                        print("Enter dual-chuck mode now? (y/n)")
//                        val answer = readLine().trim
//                        if (!answer.equalsIgnoreCase("Y") && !answer.equalsIgnoreCase("N")) {
//                            println("\t-> oops please press 'y' for yes or 'n' for no")
//                            enterDualChuckMode
//                        } else {
//                            answer.equalsIgnoreCase("Y")
//                        }
//                    }
//                    if (enterDualChuckMode) {
//                        getWiiActorRef ! WiiMoteCommands.EnterDualChukMode
//                    }
                }
                //prompt
            }
            case "SWITCH MODE" => {
                val count = getWiiMoteCount
                if (count > 1) {
                    println("WiiMote Controll Modes")
                    println("\t1) Single WiiMote Control")
                    println("\t2) Dual WiiMote Nunchuck Control")
                    def getOption :String = {
                        print("Please enter the number for the control mode you want: ")
                        val option = readLine()
                        if (!option.equals("1") && !option.equals("2")) {
                            println("\t -> oops unknown option "+option)
                            getOption
                        } else {
                            option
                        }
                    }
                    getOption match {
                        case "1" => getWiiActorRef ! WiiMoteCommands.EnterSingleChukMode
                        case "2" => getWiiActorRef ! WiiMoteCommands.EnterDualChukMode
                    }
                } else {
                    if (count == 1) {
                        println("You only have 1 remote and must remain in single-chuk mode")
                    } else {
                        println("You have not added any WiiMotes type `add wiimote` to add a WiiMote")
                    }
                }
                prompt
            }
            
            case "LAND" => { 
                getDroneActorRef ! Land
                prompt
            }
            case "TAKE OFF" => { 
                getDroneActorRef ! TakeOff
                prompt
            }
            case "RECOVER" => { 
                getDroneActorRef ! RecoverFromEmergencyMode
                prompt
            }
            case "RESET" => {
                getDroneActorRef ! ResetConnection
                prompt
            }
            case "SHUTDOWN DRONE" => {
                shutdownDrone
                prompt
            }
            case "START DRONE" => {
                getDroneActorRef ! "start"
                prompt
            }
            case "ACK" => prompt
            case ""  => prompt
            case x:Any => println("Unknown Keyboard command: "+x)
        }  
    }
    
    def isDroneFlying :Boolean = {
      val isFlyingFuture = getDroneActorRef !!! IsFlying
      isFlyingFuture.await.result.get.asInstanceOf[Boolean]
    }
    
    def prompt = {
        print("\nEnter Command: ")
    }
    
    def turnDirectionCommand(direction:String) {
        direction match {
            //TODO
            case "LEFT" => println("You said Turn Left")
            case "RIGHT" => println("You said Turn Right")
            case x:Any => println("I don't know what direction "+x+" is")
        }
    }
    
    def goDirectionCommand(direction:String) {
        direction match {
            //TODO 
            case "UP" => println("You said GO UP")
            case "DOWN" => println("You said GO DOWN")
            case "RIGHT" => println("You said GO RIGHT")
            case "LEFT" => println("You said GO LEFT")
        }
    }
    
    def printHelp = {
        println("Help?")
    }
    
    def shutdownDrone() {
        droneActor ! Land
        Thread.sleep(30L)
        droneActor ! "shutdown"
        Thread.sleep(30L)
    }
    
    def shutdownApplication() {
        print(" -> This will attempt to land the drone and quit the application.  Continue? (y/n): ")
        val answer = readLine();
        if (answer.equalsIgnoreCase("Y")) {
            shutdownDrone
            System.exit(0)
        }
    }
    
    def getWiiMoteCount = {
        val countFuture = wiiMoteActor !!! WiiMoteCommands.GetWiiMoteCount
        countFuture.await.result.get.asInstanceOf[Int]
    }
}

object KeyboardController {
    private val INSTANCE = new AtomicReference[KeyboardController](null)
    private val log = Logger.getLogger(KeyboardController.getClass)
    
    private[controller] def initialize(droneActor :ActorRef, wiiMoteActor :ActorRef) = {
        if (INSTANCE.get == null) {
            INSTANCE.synchronized {
                if (INSTANCE.get == null) {
                    INSTANCE.set(new KeyboardController(droneActor, wiiMoteActor))
                }
            }
        }
    }
    
    private[controller] def processCommand(cmd :String) {
        INSTANCE.get.processCommand(cmd)
    }
}