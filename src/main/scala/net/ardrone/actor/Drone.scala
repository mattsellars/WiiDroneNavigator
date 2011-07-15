package net.ardrone.actor

import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.actor.Actor._
import java.net.DatagramPacket
import java.net.DatagramSocket
import java.net.InetAddress
import java.net.InetSocketAddress
import java.util.concurrent.PriorityBlockingQueue
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import org.apache.log4j.Logger
import net.ardrone.model.DroneCommands
import net.ardrone.model.DroneRobotCommunicationCommands._
import net.ardrone.model.DroneActorMoveMentCommands._
import net.ardrone.model.DroneQueryCommands._
import net.ardrone.model.DroneSystemCommands._
import net.ardrone.model.WiiMoteCommands
import java.lang.StringBuilder
import net.ardrone.model.TerminalCommands._
import net.ardrone.actor.util.ActorUtils._
import java.nio.channels._
import java.nio.ByteBuffer
import scala.collection.JavaConversions
import com.codeminders.ardrone.video.BufferedVideoImage
import java.awt.image.BufferedImage
import net.ardrone.ui.VideoImageListener

/**
 * Actor handles drone communications
 * @author Matt Sellars
 *
 */
class DroneMovementActor extends Actor {
    def receive = {
        case TakeOff => { Drone.getDroneConnection.takeOff }
        case Land => { Drone.getDroneConnection.land }
        case TurnLeft => { Drone.getDroneConnection.left }
        case TurnRight => { Drone.getDroneConnection.right }
        case GoUp => { Drone.getDroneConnection.gazUp }
        case GoDown => { Drone.getDroneConnection.gazDown }
        case StopMovement => { Drone.getDroneConnection.stopMovement }
        case ResetConnection => { Drone.createNewDroneInstance }
        case SetPitch(pitch) => { Drone.getDroneConnection.setPitch(pitch) }
        case SetRoll(roll) => { Drone.getDroneConnection.setRoll(roll) }
        case SetGaz(gaz) => { Drone.getDroneConnection.setGaz(gaz) }
        case SetYaw(yaw) => { Drone.getDroneConnection.setYaw(yaw) }
        case SetPitchAndRoll(pitch, roll) => {
            Drone.getDroneConnection.setPitch(pitch)
            Drone.getDroneConnection.setRoll(roll)
        }
        case FlatTrim => { Drone.getDroneConnection.setFlatTrim }
        case IsFlying => { Drone.getDroneConnection.isFlying }
        case RecoverFromEmergencyMode => { Drone.getDroneConnection.recoverEmergency }
        case SetOutDoorMode => { Drone.getDroneConnection.setOutDoorMode }
        case SetInDoorMode => { Drone.getDroneConnection.setInDoorMode }
        case SetUsingInDoorHull => { Drone.getDroneConnection.setUsingInDoorHull }
        case SetUsingOutDoorHull => { Drone.getDroneConnection.setUsingOutDoorHull }
        case AddVideoImageListener(listener) => { Drone.addVideoImageListener(listener) }
        case RemoveVideoImageListener(listener) => { Drone.addVideoImageListener(listener) }
        case StartVideo => { Drone.startDroneVideo }
        case StopVideo => { Drone.stopDroneVideo }
        case cmd:VideoChanelConfigCommand => { Drone.getDroneConnection.sendVideoChannelCommand(cmd) }
        case SetAltitudeMax(altMaxMillimeters) => { Drone.getDroneConnection.setMaxAltitude(altMaxMillimeters) }
        case "shutdown" => { Drone.shutdownDrone }
        case "start" => { Drone.startDrone }
        case "debug flying" => { Drone.setDebugFlying(true) }
        case "debug not flying" => { Drone.setDebugFlying(false) }
        
        case x:Any => log.warn("Unknown drone command: "+x)
    }
}

/**
 * Drone communications singleton
 */
private object Drone {
    val log = Logger.getLogger(Drone.getClass)
    
    private val appRunning :AtomicBoolean = new AtomicBoolean(true)
    private val FLIGHT_CONTROLS = new AtomicReference[DroneFlightControlsThread](new DroneFlightControlsThread(1))
    private val NAV_DATA = new AtomicReference[DroneFlightNavDataThread](new DroneFlightNavDataThread(FLIGHT_CONTROLS.get))
    private val VIDEO_DATA = new AtomicReference[DroneVideoDataThread](new DroneVideoDataThread)
    private val VIDEO_IMAGE_LISTENERS = new AtomicReference[List[VideoImageListener]](List[VideoImageListener]())
    
    def setDebugFlying(flag:Boolean) {
        getDroneConnection.setFlying(flag)
    }
    
    def getDroneAddress :InetAddress = {
        val addrBytes = List(new java.lang.Integer(192).byteValue(),
                         new java.lang.Integer(168).byteValue(),
                         new java.lang.Integer(1).byteValue(),
                         new java.lang.Integer(1).byteValue());
        return InetAddress.getByAddress(addrBytes.toArray)
    }
    
    def getDroneConnection = {
        FLIGHT_CONTROLS.get
    }
    
    def isApplicationRunning() :Boolean = {
        appRunning.get
    }
    
    def startDrone = {
        if (!isApplicationRunning) {
            appRunning.set(true)
            FLIGHT_CONTROLS.get.startThread
            NAV_DATA.get.startThread
        }
    }
    
    def shutdownDrone  = {
        if (isApplicationRunning) {
            appRunning.set(false)
            FLIGHT_CONTROLS.get.shutdownThread
            NAV_DATA.get.shutdownThread
        }
    }
    
    def startDroneVideo = {
        getDroneConnection.startVideo
        VIDEO_DATA.get.startVideo
        getDroneConnection.sendVideoChannelCommand(DroneCommands.DRONE_SET_VIDEO_CHANNEL_HZ_ONLY)
    }
    
    def stopDroneVideo = {
        getDroneConnection.stopVideo
        VIDEO_DATA.get.stopVideo
    }
    /**
     * Add a new video image listener
     */
    def addVideoImageListener(listener:VideoImageListener) :Unit = {
        VIDEO_IMAGE_LISTENERS.synchronized {
            val listeners = VIDEO_IMAGE_LISTENERS.get
            VIDEO_IMAGE_LISTENERS.set(listeners :+ listener)
        }
    }
    
    /**
     * Remove a video image listener from the list
     */
    def removeVideoImageListener(toRemove:VideoImageListener) :Unit = {
        VIDEO_IMAGE_LISTENERS.synchronized {
            val listeners = VIDEO_IMAGE_LISTENERS.get
            VIDEO_IMAGE_LISTENERS.set(listeners.filter({listener:VideoImageListener => listener != toRemove}))
        }
    }
    
    def newVideoImageEvent(image:BufferedImage) :Unit = {
        for (listener <- VIDEO_IMAGE_LISTENERS.get) {
            listener.newVideoImage(image)
        }
    }
    
    protected[actor] def createNewDroneInstance() {
        val oldComThread = FLIGHT_CONTROLS.get
        try {
            oldComThread.shutdownThread()
        } catch {
            case t :Throwable => log.error("Error shutting down old flight movement communications thread", t)
        }
        FLIGHT_CONTROLS.set(new DroneFlightControlsThread(oldComThread.getNextSequence))
    }
    
    /**
     * Communication Thread for giving  flight commands to ARDrone
     */
    class DroneFlightControlsThread(curCommandSeq:Int) extends Runnable {
        private val log :Logger = Logger.getLogger("FLIGHTCONTROLS")
        private val commandQueue = new ArrayBlockingQueue[DroneCommand](64);
        /* CONSTANTS */
        private val droneAddress = Drone.getDroneAddress
        
        /**
         * TODO make these uler values configurable? e.g. sensitivity
         */
        private val turnEuler :Float = 0.7F
        private val altitudeEuler :Float = 0.7F
        
        private val COMMAND_INTERVAL = 30L;
        
        /* FLIGHT VARS */
        private val pitch = new AtomicReference[Float](0F)
        private val roll   = new AtomicReference[Float](0F)
        private val gaz    = new AtomicReference[Float](0F)
        private val yaw    = new AtomicReference[Float](0F)
        
        private val lastMovementCommand = new AtomicReference[MovementUpdateCommand](null)
        private val lastMovementCommandCount = new AtomicInteger(0)
        
        private val flying :AtomicBoolean = new AtomicBoolean(false)
        //private val resetSequence   :AtomicReference[Float] = new AtomicReference[Float](1F)
        
        private val commandSequence = new AtomicInteger(curCommandSeq);
        private val socket = new DatagramSocket(); //communications socket
        socket.setSoTimeout(3000);
        Runtime.getRuntime().addShutdownHook(new Thread(new Runnable(){
            def run = {
                if (null != socket && !socket.isClosed) socket.close()
            }
        }))
        
        var threadDrone:Thread = null;
        startThread
        
        def run = {
            log.info("Starting Flight Control Thread")
            try {
                while(isApplicationRunning && !Thread.interrupted()){
                    updateDroneMovementValues();
                    Thread.sleep(COMMAND_INTERVAL);
                    try {
                        val cmd = commandQueue.poll(COMMAND_INTERVAL, TimeUnit.MILLISECONDS)
                        if (null != cmd) {
                            val buffer = cmdToString(cmd).getBytes()
                            val packet = new DatagramPacket(buffer, buffer.length, droneAddress, 5556)
                            socket.send(packet);
                        }
                    } catch {
                        case t:Throwable => log.error("Unable to send message to drone", t)
                        case _ => log.error("Unknown error occurred!!") 
                    }
                }
            } catch {
                case t: Throwable => {
                    if (isApplicationRunning) {
                        log.error("Running flight communications thread hard failure attempting to restart flight communications thread", t);
                        Drone.createNewDroneInstance
                    }
                }
            }
            log.info("Drone flight communications thread stopped");
        }
        
        def isFlying = flying.get
        
        protected[Drone] def setFlying(isFlying:Boolean) :Unit = {
            this.flying.set(isFlying)
        }
        
        def setPitch(newPitch:Float) {
            if (newPitch < -1) {
                pitch.set(-1);
            } else if (newPitch > 1) {
                pitch.set(1)
            } else if (newPitch >= -1 && newPitch <= 1)
                pitch.set(newPitch)
            else {
                if (log.isDebugEnabled()) log.debug("Not Obeying Pitch Value: "+newPitch)
            }
        }
        def sendVideoChannelCommand(cmd:VideoChanelConfigCommand) {
            sendAtCmd(cmd)
        }
        def setRoll(newRoll:Float) {
            if (newRoll < -1) {
                roll.set(newRoll)
            } else if (newRoll > 1) {
                roll.set(1)
            } else if (newRoll >= -1 && newRoll <= 1)
                roll.set(newRoll)
            else {
                if (log.isDebugEnabled()) log.debug("Not Obeying Roll Value:"+newRoll)
            }
        }
        
        def setPitchAndRoll(newPitch:Float, newRoll:Float) {
            setPitch(newPitch)
            setRoll(newRoll)
        }
        
        def left = { yaw.set(-turnEuler) }
        def right = { yaw.set(turnEuler) }
        
        /**
         * Updated for analog stick mode - since the stick is some times not 0 when
         * not in use it's not perfectly centered... this can cause the drone to drift
         * so we add a slop range of 0.2 to -0.2 
         * TODO make this range configurable?
         */
        def setGaz(newGaz:Float) = { 
            if (newGaz > -1 && newGaz < 1) {
                if (newGaz < 0.2 && newGaz > -0.2) {
                    gaz.set(0)
                } else {
                    gaz.set(newGaz)
                }
            }
        }
        
        /**
         * Updated for analog stick mode - since the stick is some times not 0 when
         * not in use it's not perfectly centered... this can cause the drone to drift
         * so we add a slop range of 0.2 to -0.2 
         * TODO make this range configurable?
         */
        def setYaw(newYaw:Float) = {
            if (newYaw > -1 && newYaw < 1) {
                if (newYaw < 0.2 && newYaw > -0.2) {
                    yaw.set(0)
                } else {
                    yaw.set(newYaw)
                }
            }
        }
        
        def gazUp = {
            gaz.set(altitudeEuler)
        }
        def gazDown = {
            gaz.set(-altitudeEuler)
        }
        
        def stopMovement = {
            gaz.set(0)
            yaw.set(0)
        }
        
        def land = {
            log.info("Recieved Land Command")
            sendAtCmd(DroneCommands.DRONE_LAND)
        }
        
        def takeOff = {
            log.info("Recieved Take Off Command")
            sendAtCmd(DroneCommands.DRONE_TAKE_OFF)
            stopMovement
            Thread.sleep(COMMAND_INTERVAL)
            setFlatTrim
        }
        
        def setFlatTrim = {
            log.info("Setting Flat Trim info")
            sendAtCmd(DroneCommands.DRONE_REGISTER_FLAT_TRIM)
        }
        
        def recoverEmergency = {
            log.info("Sending emergency recover command")
            sendAtCmd(DroneCommands.DRONE_RECOVER_EMERGENCY)
        }
        
        def setOutDoorMode {
            log.info("Configuring drone to outdoor mode")
            sendAtCmd(DroneCommands.DRONE_OUT_DOOR_MODE)
        }
        
        def setInDoorMode {
            log.info("Configuring drone to indoor mode")
            sendAtCmd(DroneCommands.DRONE_IN_DOOR_MODE)
        }
        
        def setUsingInDoorHull {
            log.info("Configuring drone for indoor hull")
            sendAtCmd(DroneCommands.DRONE_USE_IN_DOOR_HULL)
        }
        
        def setUsingOutDoorHull {
            log.info("Configuring drone for outdoor hull")
            sendAtCmd(DroneCommands.DRONE_USE_OUT_DOOR_HULL)
        }
        
        def resetCmdWatchDog {
            log.info("Resetting Command Watch Dog")
            //commandSequence.set(1)
            sendAtCmd(DroneCommands.DRONE_RESET_WATCHDOG)
        }
        
        def startVideo {
            log.info("Starting Video")
            sendAtCmd(new VideoOnConfigCommand)
        }
        
        def stopVideo {
            log.info("Stopping Video")
            sendAtCmd(new VideoOffConfigCommand)
        }
        
        def setMaxAltitude(maxAlt:Int) {
            log.info("Setting altitude_max to "+maxAlt)
            sendAtCmd(new AltitudeMaxConfig(maxAlt))
        }
        
        private[actor] def shutdownThread() {
            if (isApplicationRunning) {
                log.info("Shutting down drone flight control thread")
                try {
                    socket.close
                } catch {
                    case t:Throwable => {} //ignore
                }
                Thread.sleep(40L)
            }
        }
        
        private[actor] def startThread() {
            log.info("Starting drone flight control thread")
            try {
                threadDrone = new Thread(this)
                threadDrone.setName("ARDrone:"+droneAddress);
                threadDrone.start();
            } catch {
                case t:Throwable => log.error("Error starting drone connection", t)
            }
        }
        protected[actor] def getNextSequence() :Int = {
            commandSequence.getAndIncrement()
        }
        private def updateDroneMovementValues() {
            if (flying.get) {
                val moveCmd = new MovementUpdateCommand(pitch.get, roll.get, gaz.get, yaw.get) 
                if(moveCmd != lastMovementCommand.get || lastMovementCommandCount.incrementAndGet >= 5) {
                    lastMovementCommand.set(moveCmd)
                    lastMovementCommandCount.set(0)
                    sendAtCmd(moveCmd)
                } else {
                    lastMovementCommandCount.incrementAndGet
                }
            }
        }
        
        protected[Drone] def sendAtCmd(cmd: DroneCommand) {
            if (log.isDebugEnabled) {
                log.debug(cmd)
            }
            try {
                commandQueue.offer(cmd)
            } catch {
                case t:Throwable => { log.error("received error queuing command", t) }
            }
//            try {
//                val buffer = cmdToString(cmd).getBytes()
//                val packet = new DatagramPacket(buffer, buffer.length, droneAddress, 5556)
//                socket.send(packet);
//            } catch {
//                case t:Throwable => log.error("Unable to send message to drone", t)
//                case _ => log.error("Unknown error occurred!!") 
//            }
        }
        
        private def cmdToString(cmd: DroneCommand) :String = {
            if (log.isDebugEnabled) {
                val strCmd = cmd.toDroneCmd(getNextSequence)
                log.debug(cmd +"["+strCmd.trim+"]")
                strCmd
            } else {
                cmd.toDroneCmd(getNextSequence)
            }
        }
    }
    
    
//Taken from C API comments    
// 31                                                             0
//  x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x -> state
//  | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
//  | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | FLY MASK : (0) ardrone is landed, (1) ardrone is flying
//  | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | VIDEO MASK : (0) video disable, (1) video enable
//  | | | | | | | | | | | | | | | | | | | | | | | | | | | | | VISION MASK : (0) vision disable, (1) vision enable
//  | | | | | | | | | | | | | | | | | | | | | | | | | | | | CONTROL ALGO : (0) euler angles control, (1) angular speed control
//  | | | | | | | | | | | | | | | | | | | | | | | | | | | ALTITUDE CONTROL ALGO : (0) altitude control inactive (1) altitude control active
//  | | | | | | | | | | | | | | | | | | | | | | | | | | USER feedback : Start button state
//  | | | | | | | | | | | | | | | | | | | | | | | | | Control command ACK : (0) None, (1) one received
//  | | | | | | | | | | | | | | | | | | | | | | | | Trim command ACK : (0) None, (1) one received
//  | | | | | | | | | | | | | | | | | | | | | | | Trim running : (0) none, (1) running
//  | | | | | | | | | | | | | | | | | | | | | | Trim result : (0) failed, (1) succeeded
//  | | | | | | | | | | | | | | | | | | | | | Navdata demo : (0) All navdata, (1) only navdata demo
//  | | | | | | | | | | | | | | | | | | | | Navdata bootstrap : (0) options sent in all or demo mode, (1) no navdata options sent
//  | | | | | | | | | | | | | | | | | | | | Motors status : (0) Ok, (1) Motors Com is down
//  | | | | | | | | | | | | | | | | | |
//  | | | | | | | | | | | | | | | | | Bit means that there's an hardware problem with gyrometers
//  | | | | | | | | | | | | | | | | VBat low : (1) too low, (0) Ok
//  | | | | | | | | | | | | | | | VBat high (US mad) : (1) too high, (0) Ok
//  | | | | | | | | | | | | | | Timer elapsed : (1) elapsed, (0) not elapsed
//  | | | | | | | | | | | | | Power : (0) Ok, (1) not enough to fly
//  | | | | | | | | | | | | Angles : (0) Ok, (1) out of range
//  | | | | | | | | | | | Wind : (0) Ok, (1) too much to fly
//  | | | | | | | | | | Ultrasonic sensor : (0) Ok, (1) deaf
//  | | | | | | | | | Cutout system detection : (0) Not detected, (1) detected
//  | | | | | | | | PIC Version number OK : (0) a bad version number, (1) version number is OK
//  | | | | | | | ATCodec thread ON : (0) thread OFF (1) thread ON
//  | | | | | | Navdata thread ON : (0) thread OFF (1) thread ON
//  | | | | | Video thread ON : (0) thread OFF (1) thread ON
//  | | | | Acquisition thread ON : (0) thread OFF (1) thread ON
//  | | | CTRL watchdog : (1) delay in control execution (> 5ms), (0) control is well scheduled // Check frequency of control loop
//  | | ADC Watchdog : (1) delay in uart2 dsr (> 5ms), (0) uart2 is good // Check frequency of uart2 dsr (com with adc)
//  | Communication Watchdog : (1) com problem, (0) Com is ok // Check if we have an active connection with a client
//  Emergency landing : (0) no emergency, (1) emergency
//typedef enum {
//  ARDRONE_FLY_MASK            = 1 << 0,  /*!< FLY MASK : (0) ardrone is landed, (1) ardrone is flying */
//  ARDRONE_VIDEO_MASK          = 1 << 1,  /*!< VIDEO MASK : (0) video disable, (1) video enable */
//  ARDRONE_VISION_MASK         = 1 << 2,  /*!< VISION MASK : (0) vision disable, (1) vision enable */
//  ARDRONE_CONTROL_MASK        = 1 << 3,  /*!< CONTROL ALGO : (0) euler angles control, (1) angular speed control */
//  ARDRONE_ALTITUDE_MASK       = 1 << 4,  /*!< ALTITUDE CONTROL ALGO : (0) altitude control inactive (1) altitude control active */
//  ARDRONE_USER_FEEDBACK_START = 1 << 5,  /*!< USER feedback : Start button state */
//  ARDRONE_COMMAND_MASK        = 1 << 6,  /*!< Control command ACK : (0) None, (1) one received */
//  ARDRONE_FW_FILE_MASK        = 1 << 7,  /* Firmware file is good (1) */
//  ARDRONE_FW_VER_MASK         = 1 << 8,  /* Firmware update is newer (1) */
////  ARDRONE_FW_UPD_MASK         = 1 << 9,  /* Firmware update is ongoing (1) */
//  ARDRONE_NAVDATA_DEMO_MASK   = 1 << 10, /*!< Navdata demo : (0) All navdata, (1) only navdata demo */
//  ARDRONE_NAVDATA_BOOTSTRAP   = 1 << 11, /*!< Navdata bootstrap : (0) options sent in all or demo mode, (1) no navdata options sent */
//  ARDRONE_MOTORS_MASK         = 1 << 12, /*!< Motors status : (0) Ok, (1) Motors problem */
//  ARDRONE_COM_LOST_MASK       = 1 << 13, /*!< Communication Lost : (1) com problem, (0) Com is ok */
//  ARDRONE_VBAT_LOW            = 1 << 15, /*!< VBat low : (1) too low, (0) Ok */
//  ARDRONE_USER_EL             = 1 << 16, /*!< User Emergency Landing : (1) User EL is ON, (0) User EL is OFF*/
//  ARDRONE_TIMER_ELAPSED       = 1 << 17, /*!< Timer elapsed : (1) elapsed, (0) not elapsed */
//  ARDRONE_ANGLES_OUT_OF_RANGE = 1 << 19, /*!< Angles : (0) Ok, (1) out of range */
//  ARDRONE_ULTRASOUND_MASK     = 1 << 21, /*!< Ultrasonic sensor : (0) Ok, (1) deaf */
//  ARDRONE_CUTOUT_MASK         = 1 << 22, /*!< Cutout system detection : (0) Not detected, (1) detected */
//  ARDRONE_PIC_VERSION_MASK    = 1 << 23, /*!< PIC Version number OK : (0) a bad version number, (1) version number is OK */
//  ARDRONE_ATCODEC_THREAD_ON   = 1 << 24, /*!< ATCodec thread ON : (0) thread OFF (1) thread ON */
//  ARDRONE_NAVDATA_THREAD_ON   = 1 << 25, /*!< Navdata thread ON : (0) thread OFF (1) thread ON */
//  ARDRONE_VIDEO_THREAD_ON     = 1 << 26, /*!< Video thread ON : (0) thread OFF (1) thread ON */
//  ARDRONE_ACQ_THREAD_ON       = 1 << 27, /*!< Acquisition thread ON : (0) thread OFF (1) thread ON */
//  ARDRONE_CTRL_WATCHDOG_MASK  = 1 << 28, /*!< CTRL watchdog : (1) delay in control execution (> 5ms), (0) control is well scheduled */
//  ARDRONE_ADC_WATCHDOG_MASK   = 1 << 29, /*!< ADC Watchdog : (1) delay in uart2 dsr (> 5ms), (0) uart2 is good */
//  ARDRONE_COM_WATCHDOG_MASK   = 1 << 30, /*!< Communication Watchdog : (1) com problem, (0) Com is ok */
//  ARDRONE_EMERGENCY_MASK      = 1 << 31  /*!< Emergency landing : (0) no emergency, (1) emergency */
//} def_ardrone_state_mask_t;
    
    class DroneFlightNavDataThread(val drone:DroneFlightControlsThread) extends Runnable {
        private val log = Logger.getLogger("DRONENAVDATA")
        private val NAV_STATE_OFFSET_BYTES    = 4
        private val NAV_CURRENT_SEQUENCE_VAL  = 8
        private val NAV_BATTERY_OFFSET_BYTES  = 24
        private val NAV_PITCH_OFFSET_BYTES    = 28
        private val NAV_ROLL_OFFSET_BYTES     = 32
        private val NAV_YAW_OFFSET_BYTES      = 36
        private val NAV_ALTITUDE_OFFSET_BYTES = 40
    
        private val ARDRONE_FLYING_MASK :Int       = 1 << 0
        private val ARDRONE_VIDEO_MASK  :Int       = 1 << 1
        private val ARDRONE_ALTITUDE_MASK :Int     = 1 << 4
        private val ARDRONE_ULTRASOUND_MASK :Int   = 1 << 21
        private val ARDRONE_VIDEO_THREAD_ON :Int   = 1 << 26
        private val ARDRONE_COM_WATCHDOG_MASK :Int = 1 << 30
        private val ARDRONE_EMERGENCY_MASK :Int    = 1 << 31
        
        private val recieveBufferSize = 512
        private val navDataPort = 5554
        private val droneAddress = getDroneAddress
        //private val socket = new DatagramSocket()
        private val rumbleConfig = new LowBatteryRumbler(30) //start warning at 30% battery
        
        /** NIO **/
        private val channel:DatagramChannel = DatagramChannel.open
        channel.configureBlocking(false)
        channel.socket().bind(new InetSocketAddress(navDataPort));
        channel.connect(new InetSocketAddress(getDroneAddress, navDataPort))
        private val selector:Selector = Selector.open
        channel.register(selector, SelectionKey.OP_READ | SelectionKey.OP_WRITE);
        
        var initBytes = Array[Byte](0x01, 0x00, 0x00, 0x00);//"8".getBytes //
        
        var threadDrone:Thread = null;
        startThread
     
        def iniNIOWriteRead :Unit = {
            channel.register(selector, SelectionKey.OP_READ | SelectionKey.OP_WRITE);
        }
        
        def run { 
            if (log.isDebugEnabled) 
                log.debug("Starting navData NIO processing")
            
            val inBuffer = ByteBuffer.allocate(recieveBufferSize);
            val outBuffer = ByteBuffer.allocate(initBytes.length);
            
            while(isApplicationRunning && !Thread.interrupted()) {
                try {
                    selector.select();
                    val selectorKeys = JavaConversions.asSet(selector.selectedKeys());
                    for (key <- selectorKeys) {
                        if(key.isWritable()){
                            outBuffer.clear
                            outBuffer.put(initBytes);
                            outBuffer.flip();
                            channel.write(outBuffer);
                            channel.register(selector, SelectionKey.OP_READ);
                        } else if (key.isReadable) {
                            inBuffer.clear
                            val readLen = channel.read(inBuffer);
                            val data = new Array[Byte](readLen)
                            inBuffer.flip
                            inBuffer.get(data, 0, readLen);
                            if (data.length == 24) {
                                drone.sendAtCmd(new NavDataDemoConfigCommand)
                                Thread.sleep(30l);
                                drone.sendAtCmd(new CTRLCommand)
                            } else {
                                parseNavData(data)
                            }
                        }
                    }
                } catch {
                    case t:Throwable => {
                        if (isApplicationRunning) {
                            log.warn("Received error logging navdata: "+t.getMessage, t)
                            iniNIOWriteRead
                            run
                        }
                    }
                }
            }
            
            if (log.isDebugEnabled) 
                log.debug("Ending navdata NIO processing")
        }
        
        private def parseNavData(data:Array[Byte]) :Unit = {
            var droneState :Int = parseData(data, NAV_STATE_OFFSET_BYTES)
                        
            //TODO synch received sequence with command thread sequence value
            //this will allow us to potentially reset the value.
            var commandSequence :Int = parseData(data, NAV_CURRENT_SEQUENCE_VAL)
            log.info("NavData cmd sequence: "+commandSequence)
            
            if ((droneState & ARDRONE_COM_WATCHDOG_MASK) > 0) {
                log.info("Sending Watch Dog Reset")
                drone.sendAtCmd(DroneCommands.DRONE_RESET_WATCHDOG)
            }
            
            if ((droneState & ARDRONE_FLYING_MASK) > 0) {
                if (log.isDebugEnabled) log.debug("Drone is Flying!")
                drone.setFlying(true)
            } else {
                if (log.isDebugEnabled) log.debug("Drone is not Flying")
                drone.setFlying(false)
            }
            
            if ((droneState & ARDRONE_ALTITUDE_MASK) > 0) {
                if (log.isDebugEnabled) log.debug("Altitude control on")
            } else {
                if (log.isDebugEnabled) log.debug("Altitude control off")
            }
            
            if ((droneState & ARDRONE_ULTRASOUND_MASK) > 0) {
                if (log.isDebugEnabled) log.debug("Drone ultrasound on")
            } else {
                if (log.isDebugEnabled) log.debug("Drone ultrasound off")
            }
            
            if ((droneState & ARDRONE_EMERGENCY_MASK) > 0) {
                log.info("Drone in emergency mode")
            }
            
            if ((droneState & ARDRONE_VIDEO_THREAD_ON) >0) {
                if (log.isDebugEnabled) log.debug("Drone video THREAD is on")
            } else {
                if (log.isDebugEnabled) log.debug("Drone video THREAD is off")
            }
            
            if ((droneState & ARDRONE_VIDEO_MASK) > 0) {
                if (log.isDebugEnabled) log.debug("Video is on")
            } else {
                if (log.isDebugEnabled) log.debug("Video is off")
            }
            
            val batteryLevel = parseData(data, NAV_BATTERY_OFFSET_BYTES)
            rumbleConfig.issueBatterWarningIfNeeded(batteryLevel)
            
            if (log.isDebugEnabled) {
                log.debug("Battery: "+parseData(data, NAV_BATTERY_OFFSET_BYTES))
                log.debug("Altitude: "+(parseDataAsFloat(data, NAV_ALTITUDE_OFFSET_BYTES)/1000))
                log.debug("Pitch: "+(parseDataAsFloat(data, NAV_PITCH_OFFSET_BYTES) / 1000))
                log.debug("Roll: "+(parseDataAsFloat(data, NAV_ROLL_OFFSET_BYTES) / 1000))
                log.debug("Yaw: "+(parseDataAsFloat(data, NAV_YAW_OFFSET_BYTES))/1000)
            }
        }
        
        private def parseDataAsFloat(data:Array[Byte], offset:Int) :Float = {
            java.lang.Float.intBitsToFloat(parseData(data, offset))
        }
        
        /**
         * Data comes in as little endian byte order and
         * we need to change to big endian int to work 
         * with it in Java/Scala land
         * 
         * For Example:
         * little endian 32 bit value 0F 0D 0E 0C becomes big endian 0C 0E 0D 0F
         */
        private def parseData(data:Array[Byte], offset:Int) :Int = {
            var retVal = 0
            var index = 3
            while(index>=0) {
                retVal = retVal << 8;
                retVal |= (data(offset+index) & 0xFF)
                index-=1
            }
            return retVal;
        }
        
        private[actor] def shutdownThread() {
            if (isApplicationRunning) {
                try {
                    selector.close()
                } catch {
                    case _ => {} //ignore
                }
                try {
                    channel.disconnect
                } catch {
                    case _ => {} //ignore
                }
                log.info("Shutting down drone navdata thread")
                Thread.sleep(40L)
            }
        }
        
        private[actor] def startThread() {
            println("Starting drone navdata thread")
            try {
                threadDrone = new Thread(this)
                threadDrone.setName("DroneNAVDATA:"+droneAddress);
                threadDrone.start();
            } catch {
                case t:Throwable => log.error("Error starting drone navdata connection", t)
            }
        }
        
        private class LowBatteryRumbler(val batterWarningLevelThreshold:Int) {
            private val MAX_PCT_MIN_RUMBLE_INTERVAL_MILLIS = 5000L //5 seconds between battery rumble alerts at 30% battery
            private val TWENTY_PCT_MIN_RUMBLE_INTERVAL_MILLIS = 2000L //1.5 seconds between battery rumble alerts at 30% battery
            private val TEN_PCT_MIN_RUMBLE_INTERVAL_MILLIS = 1000L //1.5 seconds between battery rumble alerts at 30% battery
            private var lastRumbleAlert:Long = 0
            
            def issueBatterWarningIfNeeded(batteryLevel:Int) :Unit = {
                if (batteryLevel <= batterWarningLevelThreshold) {
                    log.warn("LOW BATTERY ALERT: "+batteryLevel+"%")
                    val timeElapsedSineLastRumble = calculateLastRumbleTimeElapsed
                    if (calculateLastRumbleTimeElapsed > getElapsedTimeThreshold(batteryLevel)) {
                        lastRumbleAlert = System.currentTimeMillis
                        getWiiActorRef ! new WiiMoteCommands.RumbleWiiMote(3, 200L)
                    }
                }
            }
            
            private def calculateLastRumbleTimeElapsed:Long = {
                System.currentTimeMillis - lastRumbleAlert
            }
            
            private def getElapsedTimeThreshold(batteryLevel:Int) :Long = {
                if (batteryLevel < batterWarningLevelThreshold && batteryLevel > 20) {
                    MAX_PCT_MIN_RUMBLE_INTERVAL_MILLIS
                } else if (batteryLevel < 20 && batteryLevel > 10) {
                    TWENTY_PCT_MIN_RUMBLE_INTERVAL_MILLIS
                } else {
                    TEN_PCT_MIN_RUMBLE_INTERVAL_MILLIS
                }
            }
        }
    }
    
    class DroneVideoDataThread extends Runnable {
        private val recieveBufferSize = 102400
        private val videoDataPort = 5555
        private val droneAddress = getDroneAddress
        private val socket = new DatagramSocket()
        
        private val videoRunning = new AtomicBoolean(false);
        
        private val channel:DatagramChannel = DatagramChannel.open
        channel.configureBlocking(false)
        channel.socket().bind(new InetSocketAddress(videoDataPort));
        channel.connect(new InetSocketAddress(getDroneAddress, videoDataPort))
        private val selector:Selector = Selector.open
        channel.register(selector, SelectionKey.OP_READ | SelectionKey.OP_WRITE);
        
        private var initBytes = Array[Byte](0x01, 0x00, 0x00, 0x00);
        private val videoImg = new BufferedVideoImage
        def initVideoFeed :Unit = {
            log.info("Initializing Video Data stream connection")
            var buffer = Array[Byte](0x01, 0x00, 0x00, 0x00);//"8".getBytes //
            socket.send(new DatagramPacket(buffer, buffer.length, droneAddress, videoDataPort));
        }

        def runOld :Unit = {
            initVideoFeed
            while(isVideoRunning && 
                  isApplicationRunning && 
                  !Thread.interrupted()) {
                try {
                    var rawReceive = new Array[Byte](recieveBufferSize);
                    var videoData = new DatagramPacket(rawReceive, rawReceive.length, droneAddress, videoDataPort)
                    socket.receive(videoData)
                    log.info("Video data received length: "+videoData.getLength)
                } catch {
                    case t:Throwable => {
                        if (isApplicationRunning) {
                            log.warn("Received error logging video data: "+t.getMessage)
                            run
                        }
                    }
                }
            }
        }
        
        def iniNIOWriteRead :Unit = {
            channel.register(selector, SelectionKey.OP_READ | SelectionKey.OP_WRITE);
        }
        
        //NIO based data reader
        def run :Unit = {
            if (log.isDebugEnabled) 
                log.debug("Starting navData NIO processing")
            
            val inBuffer = ByteBuffer.allocate(recieveBufferSize);
            val outBuffer = ByteBuffer.allocate(initBytes.length);
            
            while(isApplicationRunning && videoRunning.get && !Thread.interrupted()) {
                try {
                    selector.select();
                    val selectorKeys = JavaConversions.asSet(selector.selectedKeys());
                    for (key <- selectorKeys) {
                        if(key.isWritable()){
                            outBuffer.clear
                            outBuffer.put(initBytes);
                            outBuffer.flip();
                            channel.write(outBuffer);
                            channel.register(selector, SelectionKey.OP_READ);
                        } else if (key.isReadable) {
                            inBuffer.clear
                            val readLen = channel.read(inBuffer);
                            if (readLen > 0) {
                                inBuffer.flip
                                //parse image data with codeminers lib
                                videoImg.addImageStream(inBuffer)
                                val image = new BufferedImage(videoImg.getWidth, videoImg.getHeight,
                                        BufferedImage.TYPE_INT_RGB);
                                image.setRGB(0, 0, videoImg.getWidth, videoImg.getHeight, videoImg.getJavaPixelData,
                                             0, videoImg.getWidth);
                                newVideoImageEvent(image)
                            }
                        }
                    }
                } catch {
                    case t:Throwable => {
                        if (isApplicationRunning) {
                            log.warn("Received error logging video data: "+t.getMessage, t)
                            iniNIOWriteRead
                            run
                        }
                    }
                }
            }
            
            if (log.isDebugEnabled) 
                log.debug("Ending videodata NIO processing")
        }
        
        def startVideo {
            if (!videoRunning.get) {
                videoRunning.set(true)
                val t = new Thread(this)
                t.start
            }
        }
        
        def stopVideo {
            videoRunning.set(false)
        }
        
        private[actor] def shutdownThread() {
            if (isApplicationRunning) {
                try {
                    selector.close()
                } catch {
                    case _ => {} //ignore
                }
                try {
                    channel.disconnect
                } catch {
                    case _ => {} //ignore
                }
                log.info("Shutting down drone video thread")
            }
        }
        
        private def isVideoRunning :Boolean = {
            return videoRunning.get
        }
    }
}