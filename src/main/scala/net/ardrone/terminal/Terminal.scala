package net.ardrone.terminal

import se.scalablesolutions.akka.dispatch.Future
import se.scalablesolutions.akka.actor.ActorRef
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.actor.Actor._
import se.scalablesolutions.akka.actor.ActorRegistry
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import org.apache.log4j.{Logger, Level};
import java.io.PrintStream
import java.io.OutputStream
import net.ardrone.model.TerminalCommands._
import net.ardrone.model.DroneActorMoveMentCommands._
import net.ardrone.model.DroneSystemCommands
import net.ardrone.model.DroneCommands._
import net.ardrone.model.DroneQueryCommands
import net.ardrone.model.WiiMoteCommands
import net.ardrone.actor.util.ActorUtils._
import net.ardrone.controller.wii.WiiControlMode
import net.ardrone.ui.VideoFrameDisplayer


class TerminalActor extends Actor {
    def receive = {
        case ControlModeUpdateAcknowedgeCommand(curMode, activeControls) => {
            curMode match {
                case WiiControlMode.SINGLE_CHUK => {
                    Terminal.printToTerminal("You are now in single WiiMote+Nunchuk mode using controller: "+activeControls)
                }
                case WiiControlMode.DUAL_CHUK => {
                    Terminal.printToTerminal("You are now in dual WiiMote+Nunchuk mode using controllers: "+activeControls)
                }
            }
        }
        case InitializeCommand(a,b) => {
            Terminal.processInput("INITIALIZE");
        }
        case cmd:String => {
            if (null != cmd) Terminal.processInput(cmd)
        }
        case PrintStatus(message) => Terminal.printToTerminal(message)
        case PrintStatusProgress(message) => Terminal.printlnToTerminalWithNoPrompt(message) 
        case x:Any => {
             Terminal.printToTerminal("Unkown terminal message recieved: "+x)
        }
    }
}

object Terminal {
    private val log = Logger.getLogger(Terminal.getClass)
    private val inputReader = new KeyboardInputReader(processInput)
    private val originalSystemOut = System.out
    
    /*
     * Redirect System.out System.err
     */
    new TerminalSystemOutStreamProxy
    new TerminalErrOutRedirector
    
    def printToTerminal(data:String) {
        originalSystemOut.println("\n"+data)
        printPrompt
    }
    
    private def printToTerminalWithNoPrompt(data :String) = {
        originalSystemOut.print(data)   
    }
    
    private[terminal] def printlnToTerminalWithNoPrompt(data :String) = {
        originalSystemOut.println(data)   
    }
    
    private def printPrompt {
        originalSystemOut.print("\n> ")
    }
    
    private[terminal] def processInput(inData:String) {
        inData.toUpperCase match {
            case loglevelCommand(logger, lvl) => {
                var level:Level = null
                lvl match {
                    case "DEBUG" => level = Level.DEBUG
                    case "INFO" => level = Level.INFO
                    case "WARN" => level = Level.WARN
                    case "ERROR" => level = Level.ERROR
                    case _ => {} //ignored for now
                }
                if (null != level) {
                    Logger.getLogger(logger).setLevel(level)
                    printToTerminal("Logger `"+logger+"` level set to: " + level)
                }
            }
            case regWiiAckCommand(name) => {
                val count = getWiiMoteCount
                if (count == 1) {
                    printToTerminal("""

Registered WiiMote """+name+""" - Ready To fly!

You can add one more WiiMote and fly in dual-chuk
mode or just take off now by pressing 1.  If you
add another WiiMote you can still toggle between
dual-chuk mode and single-chuk mode.

If at any time the drone stops reacting to the 
WiiMote, the batter dies, or for some reason you need
to stop the drone type LAND.

Fly Safe!
""")
                } else if (count == 2) {
                    printToTerminal("""

Registered second WiiMote """+name+""", type `switch mode`
to switch between single and dual WiiMote+Nunchuk control.

To Take off hit press C and Z simultaneously on the second
Nunchuk.  Once flying, pressing C and Z simultaneously on
the first Nunchuk will land the drone.
""")
                }
            }
            case regWiiMoteCommand(name) => {
                val count = getWiiMoteCount
                if (count < 2) {
                    getDroneActorRef ! DroneQueryCommands.IsFlying
                    def getName :String = {
                        if (name != null) { 
                           return name.trim
                        } else {
                           return "MOTE-"+(count+1)
                        }
                    }
                    getWiiActorRef ! "register wiimote "+getName
                } else {
                    printToTerminal("""
***************************************
* 2 is the maximum number of WiiMotes *
*  You can have too many WiiMotes :)  *
***************************************
""")
                }
            }
            case videoCommand(onOff) => {
               onOff match {
                    case "ON" => {
                        getDroneActorRef !
                            new DroneSystemCommands.AddVideoImageListener(VideoFrameDisplayer.getVideoConsumer)
                        getDroneActorRef ! DroneSystemCommands.StartVideo
                        VideoFrameDisplayer.showVideo
                    }
                    case "OFF" => {
                        VideoFrameDisplayer.hideVideo
                        getDroneActorRef !
                            new DroneSystemCommands.RemoveVideoImageListener(VideoFrameDisplayer.getVideoConsumer)
                        getDroneActorRef ! DroneSystemCommands.StopVideo
                    }
                }
               printPrompt
            }
            case "VIDEO CHANNEL" => {
                changeVideoChannel
            }
            case maxAltitudeCommand(maxAltitude) => {
                val maxAltIntVal = Integer.parseInt(maxAltitude)
                getDroneActorRef ! DroneSystemCommands.SetAltitudeMax(maxAltIntVal)
                printPrompt
            }
            case "OUTSIDE" => {
                printToTerminalWithNoPrompt("Switching drone to outdoor mode.");
                getDroneActorRef ! DroneSystemCommands.SetOutDoorMode
                switchDroneHull
                printPrompt
            }
            case "INSIDE" => {
                printToTerminal("Switching drone to indoor mode.")
                getDroneActorRef ! DroneSystemCommands.SetInDoorMode
                switchDroneHull
                printPrompt
            }
            case "FORCE FLIGHT ON" => {
                getDroneActorRef ! "debug flying"
            }
            case "FORCE FLIGHT OFF" => {
                getDroneActorRef ! "debug not flying"
            }
            case "CONTROL MODE" => {
                val count = getWiiMoteCount
                if (count == 2) {
                    printlnToTerminalWithNoPrompt("""
WiiMote Controll Modes
    1) Single WiiMote Control
    2) Dual WiiMote Nunchuck Control

""") 
                    val userOption = readUserOption("Please enter the number for the control mode you want: ",
                                                    "\t-> oops unknown option ",
                                                    Set("1", "2"))  
                    userOption match {
                        case "1" => getWiiActorRef ! WiiMoteCommands.EnterSingleChukMode
                        case "2" => getWiiActorRef ! WiiMoteCommands.EnterDualChukMode
                    }
                } else if (count > 2) {
                    printToTerminal("You can only sync 2 WiiMote+Nunchuk at a time")
                } else {
                    if (count == 1) {
                        printToTerminal("You only have 1 remote and must remain in single-chuk mode")
                    } else {
                        printToTerminal("You have not added any WiiMotes type `add wiimote` to add a WiiMote")
                    }
                }
            }
            case "HELP" => {
                printToTerminal(renderHelpInfo)
            }
            case "INITIALIZE" => {
                printToTerminal("""
Drone Navigation System Started

Type Help if you would like a list of commands and WiiControl Definitions.""")
            }
            case "LAND" => { 
                getDroneActorRef ! Land
                printPrompt
            }
            case "TAKE OFF" => { 
                getDroneActorRef ! TakeOff
                printPrompt
            }
            case "RECOVER" => { 
                getDroneActorRef ! DroneSystemCommands.RecoverFromEmergencyMode
                printPrompt
            }
            case "RESET" => {
                getDroneActorRef ! DroneSystemCommands.ResetConnection
                printPrompt
            }
            case "SHUTDOWN DRONE" => {
                shutdownDrone
                printPrompt
            }
            case "START DRONE" => {
                getDroneActorRef ! "start"
                printPrompt
            }
            case "QUIT" => {
                shutdownApplication
            }
            case "TEST RUMBLE" => {
                getWiiActorRef ! new WiiMoteCommands.RumbleWiiMote(3, 200L)
                printPrompt
            }
            case "" => printPrompt
            case x:Any => {
                printlnToTerminalWithNoPrompt("Unkown terminal command: " + inData+"\n")
                printToTerminal("Type help for the help menu.")
            }
        }
    }
    
    def switchDroneHull :Unit = {
        printToTerminalWithNoPrompt("""
What type of hull is on the drone?
    1) Indoor protective hull (blades protected)
    2) Outdoor light weight hull (blades exposed)

""")
        val userOption = readUserOption("Please enter the number for the drone hull type in use: ",
                                        "\t-> oops unknown option ",
                                        Set("1", "2"))
        userOption.trim match {
            case "1" => getDroneActorRef ! DroneSystemCommands.SetUsingInDoorHull
            case "2" => getDroneActorRef ! DroneSystemCommands.SetUsingOutDoorHull
            case x:String => printToTerminalWithNoPrompt("ERROR unknown option: "+x)
        }
        printPrompt
    }
    
    def readUserOption(normalMessage :String, onErrorInputMessage :String, options:Set[String]) :String = {
        printToTerminalWithNoPrompt(normalMessage)
        val option = readLine()
        if (!options.contains(option)) {
            printlnToTerminalWithNoPrompt(onErrorInputMessage + option)
            return readUserOption(normalMessage, onErrorInputMessage, options)
        } else {
            return option
        }
    }
    
    def changeVideoChannel {
        printToTerminalWithNoPrompt("""
Available Camera Feed Channels:
    1) Horizontal camera (default)
    2) Vertical camera
    3) Large horizontal Camera with small vertical camera view (picture in picture)
    4) Large vertical camera with small horizontal camera view (picture in picture)

""")
        val answer = readUserOption("Enter the number for the video feed you want: ",
                                    "\t-> oops please enter valid option, you entered ",
                                    Set("1", "2", "3", "4"))
        
        answer.trim match {
            case "1" => { getDroneActorRef ! DRONE_SET_VIDEO_CHANNEL_HZ_ONLY }
            case "2" => { getDroneActorRef ! DRONE_SET_VIDEO_CHANNEL_VRT_ONLY }
            case "3" => { getDroneActorRef ! DRONE_SET_VIDEO_CHANNEL_HZ_SMVRT }
            case "4" => { getDroneActorRef ! DRONE_SET_VIDEO_CHANNEL_VRT_SMHZ }
            case x:String => printToTerminalWithNoPrompt("ERROR unknown option")
        }
        printPrompt
    }
    
    def shutdownApplication() {
        printToTerminalWithNoPrompt(" -> This will attempt to land the drone and quit the application.  Continue? (y/n): ")
        val answer = readLine();
        if (answer.equalsIgnoreCase("Y")) {
            try {
                getWiiActorRef ! "QUIT"
                shutdownDrone
                ActorRegistry.shutdownAll()
            } finally {
                System.exit(0)
            }
        } else {
            printPrompt
        }
    }
    
    def shutdownDrone() {
        getDroneActorRef ! Land
        Thread.sleep(30L) // let pending commands clear
        getDroneActorRef ! "shutdown"
        Thread.sleep(30L)
    }
    
    def getWiiMoteCount = {
        val countFuture = getWiiActorRef !!! WiiMoteCommands.GetWiiMoteCount
        countFuture.await.result.get.asInstanceOf[Int]
    }
    
    def renderHelpInfo = {
        """
Help:  (Commands are not case-sensitive)
=========================================================================================
 Command           || Description
=========================================================================================
   HELP            ||   This menu... duh winning
`````````````````````````````````````````````````````````````````````````````````````````
   TAKE OFF        ||   Makes the Drone take off
`````````````````````````````````````````````````````````````````````````````````````````
   LAND            ||   Makes the Drone land
`````````````````````````````````````````````````````````````````````````````````````````
   RECOVER         ||   Use this command if you crash your drone and the 
                   ||   drone is in emergency mode(all LEDs red)
`````````````````````````````````````````````````````````````````````````````````````````
   RESET           ||   Attempts to restart communications with the drone
`````````````````````````````````````````````````````````````````````````````````````````
   SHUTDOWN DRONE  ||   Terminates the drone communication thread. 
`````````````````````````````````````````````````````````````````````````````````````````
   START DRONE     ||   Starts drone communication thread
`````````````````````````````````````````````````````````````````````````````````````````
   QUIT            ||   Exits the application
`````````````````````````````````````````````````````````````````````````````````````````
   CONTROL MODE    ||   Allows switching from single-chuk to dual-chuk control modes
                   ||   if 2 WiiMotes are connected
`````````````````````````````````````````````````````````````````````````````````````````
   VIDEO ON        ||   Opens window showing drone video with selectable camera 
                   ||   view channels
`````````````````````````````````````````````````````````````````````````````````````````
   VIDEO OFF       ||   Closes video window and stops video data consumption
`````````````````````````````````````````````````````````````````````````````````````````
   ADD WIIMOTE     ||   Bluetooth sync a wii mote to control the drone.
                   ||   If you sync one WiiMote+NunChuk you will 
                   ||   automatically be running in single-chuk mode to
                   ||   control the drone.  You can add a second WiiMote+Chuk
                   ||   and run in dual-chuk mode.  Later on you can use the CONTROL MODE
                   ||   command to switch modes when you have connected 2 WiiMote+Nunchuks
                   ||     
                   ||     
                   ||   Single-Chuk Mode WiiMote Controlls:
                   ||     WiiMote:
                   ||       Arrow Up:     Move Drone Up In Altitude
                   ||       Arrow Down:   Move Drone Down In Altitude
                   ||       Arrow Left:   Rotate Drone left
                   ||       Arrow Right:  Rotate Drone Right
                   ||       1 Button:     Take Off
                   ||       2 Button:     Land Drone
                   ||       A Button:     Resets the drone communications thread.  Try
                   ||                     this if your drone becomes unresponsive.
                   ||       Home Button:  Recover in case of crash - If you crash the drone
                   ||                     it will enter emergency mode with all red LEDs.
                   ||                     Hitting the home button will send the recover
                   ||                     command to the drone.  The LEDs should go green
                   ||                     when the drone is ready after receiving this
                   ||                     command
                   ||
                   ||     Nunchuk:
                   ||       Joystick [forward]   moves drone forwards
                   ||       Joystick [Left]      moves drone to the left (strafe left)
                   ||       Joystick [Right]     moves drone to the right (strafe right)
                   ||       Joystick [Down]      moves drone backwards
                   ||     
                   ||         
                   ||   Dual-Chuk Mode WiiMote Controlls:
                   ||     WiiMote[1] and Nunchuk[1] are one connected pair but refered to separately.
                   ||     Same goes for  WiiMote[2] and Nunchuk[2].
                   ||       
                   ||     WiiMote [1]:
                   ||       Home Button:  Recover in case of crash - If you crash the drone
                   ||                     it will enter emergency mode with all red LEDs.
                   ||                     Hitting the home button will send the recover
                   ||                     command to the drone.  The LEDs should go green
                   ||                     when the drone is ready after receiving this
                   ||                     command
                   ||
                   ||     WiiMote [2]:
                   ||       Home Button:  Recover in case of crash - If you crash the drone
                   ||                     it will enter emergency mode with all red LEDs.
                   ||                     Hitting the home button will send the recover
                   ||                     command to the drone.  The LEDs should go green
                   ||                     when the drone is ready after receiving this
                   ||                     command
                   ||
                   ||     Nunchuk [1]:
                   ||       C + Z                Lands Drone
                   ||       Z                    Move Drone Down In Altitude
                   ||       Joystick [forward]   moves drone forwards
                   ||       Joystick [Left]      moves drone to the left (strafe left)
                   ||       Joystick [Right]     moves drone to the right (strafe right)
                   ||       Joystick [Down]      moves drone backwards
                   ||       
                   ||     Nunchuk [2]:
                   ||       C + Z                Take Off
                   ||       Z                    Move Drone Up In Altitude
                   ||       Joystick [forward]   None
                   ||       Joystick [Left]      Rotate Drone left
                   ||       Joystick [Right]     Rotate Drone Right
                   ||       Joystick [Down]      None
`````````````````````````````````````````````````````````````````````````````````````````

For Full flight functionality a WiiNunchuk is required per WiiMote.  If the Nunchuk is
not responsive try unplugging it from the WiiMote and plugging it back in.
"""
    }
    
    /*
     * Input stream redirectors
     */
    private class TerminalSystemOutStreamProxy extends PrintStream(new VoidOutputStream) {
        System.setOut(this)
        override def println(x:String) = Terminal.printToTerminal(x)
        override def print(x:String) = println(x)
    }
    
    private class TerminalErrOutRedirector extends PrintStream(new VoidOutputStream) {
        System.setErr(this)
        override def println(x:String) = log.warn(x)
        override def print(x:String) = log.warn(x)
    }
    
    private class VoidOutputStream extends OutputStream {
        def write(b :Int) = {}
    }
    
    /**
     * Keyboard reader thread
     */
    private class KeyboardInputReader(val callBack: (String)=>Unit) extends Runnable {
        val running = new AtomicBoolean(true)
        val t = new Thread(this)
        t.setName("KeybaordInputReader");
        t.start();
        
        def run {
            try {
                while(running.get) {
                    callBack(readLine())
                }
            } catch {
                case t: Throwable => {
                    log.error("Error processing command", t)
                    run
                }
            }
        }
    }
}