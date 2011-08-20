package net.ardrone.model

import java.lang.StringBuilder
import java.lang.Comparable
import java.lang.Integer
import java.lang.Class
import se.scalablesolutions.akka.actor.ActorRef
import net.ardrone.controller.wii.WiiControlMode._
import net.ardrone.ui.VideoImageListener

object DroneCommands {
    /**
     * Singleton Drone commands that don't need to be instanciated.  
     * These are more inline with builders
     */
    val DRONE_TAKE_OFF = new DroneRobotCommunicationCommands.TakeOffCommand
    val DRONE_LAND = new DroneRobotCommunicationCommands.LandCommand
    val DRONE_RESET_WATCHDOG = new DroneRobotCommunicationCommands.ResetCommandWatchDogCommand
    val DRONE_RECOVER_EMERGENCY = new DroneRobotCommunicationCommands.RecoverFromEmergencyCommand
    val DRONE_REGISTER_FLAT_TRIM = new DroneRobotCommunicationCommands.RegisterFlatTrimCommand
    val DRONE_OUT_DOOR_MODE = new DroneRobotCommunicationCommands.OutDoorConfigCommand
    val DRONE_IN_DOOR_MODE = new DroneRobotCommunicationCommands.InDoorConfigCommand
    val DRONE_USE_OUT_DOOR_HULL = new DroneRobotCommunicationCommands.OutDoorHullConfigCommand
    val DRONE_USE_IN_DOOR_HULL = new DroneRobotCommunicationCommands.InDoorHullConfigCommand
    val DRONE_SET_VIDEO_CHANNEL_HZ_ONLY = new DroneRobotCommunicationCommands.HorizontalVideoChannelConfigCommand
    val DRONE_SET_VIDEO_CHANNEL_VRT_ONLY = new DroneRobotCommunicationCommands.VerticalVideoChannelConfigCommand
    val DRONE_SET_VIDEO_CHANNEL_HZ_SMVRT = new DroneRobotCommunicationCommands.LargeHorizontalSmallVerticalVideoChannelConfigCommand
    val DRONE_SET_VIDEO_CHANNEL_VRT_SMHZ = new DroneRobotCommunicationCommands.LargeVerticalSmallHorizontalVideoChannelConfigCommand
}

package DroneActorMoveMentCommands {
    case class TurnLeft
    case class TurnRight
    case class GoUp
    case class GoDown
    case class TakeOff
    case class FlatTrim
    case class Land
    case class StopMovement
    case class SetPitch(val pitch:Float)
    case class SetRoll(val roll:Float)
    case class SetGaz(val gaz:Float)
    case class SetYaw(val yaw:Float)
    case class SetPitchAndRoll(val pitch:Float, val roll:Float)
}

object TerminalCommands {
    val loglevelCommand = """LOG ([a-zA-Z.]+) ([a-zA-Z]+)""".r
    val turnCommand = """TURN ([A-Z]+)""".r
    val goCommand = """GO ([A-Z]+)""".r
    val regWiiMoteCommand = """ADD WIIMOTE( [-a-zA-Z0-9]+)?""".r
    val regWiiAckCommand = """WIIMOTE ([-a-zA-Z0-9]+) REGISTERED""".r
    val videoCommand = """VIDEO (ON|OFF)""".r
    val maxAltitudeCommand = """MAX ALT ([0-9]+)""".r
    case class TerminalStartUp
    case class InitializeCommand(val droneActor :ActorRef, val wiiMoteActor :ActorRef)
    case class ControlModeUpdateAcknowedgeCommand(val mode:WiiControlMode, val activeControllerNames:String)
    case class PrintStatus(val message:String) //prompt will print after this message
    case class PrintStatusProgress(val message:String) //print will not cause a prompt
}

package WiiMoteCommands {
    case class StartBlueTooth
    case class GetWiiMoteCount
    case class EnterDualChukMode
    case class EnterSingleChukMode
    case class GetCurrentControlMode
    case class RumbleWiiMote(val numberOfRumbles:Int, val rumbleTimeInMillis:Long)
}

package DroneQueryCommands {
    case class IsFlying
    case class IsConnected
}

package DroneSystemCommands {
    case class ResetConnection
    case class RecoverFromEmergencyMode
    case class SetOutDoorMode
    case class SetInDoorMode
    case class SetUsingInDoorHull
    case class SetUsingOutDoorHull
    case class AddVideoImageListener(listener:VideoImageListener)
    case class RemoveVideoImageListener(listener:VideoImageListener)
    case class StartVideo
    case class StopVideo
    case class SetAltitudeMax(val maxInMillimiters:Int) //10000 = unlimited, 500-5000 normal range
}

package DroneRobotCommunicationCommands {
    trait ComparableCommand extends Comparable[ComparableCommand] {
        
        override def compareTo(to:ComparableCommand) :Int = {
            commandPriority().compareTo(to.commandPriority())
        }
        
        def commandPriority() :Integer = {
            return new Integer(1) 
        }
    }
    
    trait DroneCommand extends ComparableCommand {
        def getCommandBeforeSequence : String
        def getCommandAfterSequence :String
        
        def toDroneCmd(seq:Int): String = {
            val builder = new StringBuilder().append(getCommandBeforeSequence)
            .append(seq).append(getCommandAfterSequence).append("\r")
            builder.toString
        }
    }
    
    case class MovementUpdateCommand(val pitch:Float, val roll:Float, val gaz:Float, val yaw:Float) extends DroneCommand {
        def getCommandBeforeSequence = "AT*PCMD="
        def getCommandAfterSequence() :String = {
            val builder = new StringBuilder().append(",1,")
                .append(java.lang.Float.floatToIntBits(pitch)).append(",") 
                .append(java.lang.Float.floatToIntBits(roll)).append(",") 
                .append(java.lang.Float.floatToIntBits(gaz)).append(",") 
                .append(java.lang.Float.floatToIntBits(yaw));
            return builder.toString()
        }
    }
    
    class CTRLCommand() extends DroneCommand {
        def getCommandBeforeSequence : String = ""
        def getCommandAfterSequence :String = ""
        override def toDroneCmd(seq:Int): String = {
            "AT*CTRL=1"
        }
    }
    
    abstract class BuiltInCommand extends DroneCommand {
        def getCommandBeforeSequence = "AT*REF="
    }
    
    case class TakeOffCommand extends BuiltInCommand {
        override def getCommandAfterSequence =",290718208"
    }
    
    case class LandCommand extends BuiltInCommand {
        override def getCommandAfterSequence =  ",290717696"
    }
    
    case class RecoverFromEmergencyCommand() extends BuiltInCommand {
        override def getCommandAfterSequence =  ",290717952"
    }
    
    abstract class NoParamCommand extends DroneCommand {
        def getCommandAfterSequence() :String = ""
    }
    
    case class RegisterFlatTrimCommand() extends NoParamCommand {
        def getCommandBeforeSequence = "AT*FTRIM="
    }
    
    case class ResetCommandWatchDogCommand() extends NoParamCommand {
        def getCommandBeforeSequence = "AT*COMWDG="
    } 
    
    abstract class ConfigCommand() extends DroneCommand {
        def getCommandBeforeSequence = "AT*CONFIG="
    }
    
    abstract class ControlConfigCommand extends ConfigCommand {
        def controlName() :String
        def controlValue() :String
        override def getCommandAfterSequence =",\"control:" + controlName +"\",\""+controlValue+"\""
    }
    
    abstract class VideoConfigCommand extends ConfigCommand {
        def videoParamName() :String
        def videoParamValue() :String
        override def getCommandAfterSequence =",\"video:" + videoParamName +"\",\""+videoParamValue+"\""   
    }
    
    abstract class GeneralConfigCommand extends ControlConfigCommand {
        override def getCommandAfterSequence =",\"general:" + controlName +"\",\""+controlValue+"\""
    }
    
    case class AltitudeMaxConfig(altVal:Int) extends ControlConfigCommand {
        def controlName = "altitude_max"
        def controlValue = altVal.toString
        //override def getCommandAfterSequence = ",\"control:altitude_max\",\""+altVal+"\"" //TODO
    }
    
    case class OutDoorConfigCommand() extends ControlConfigCommand {
        def controlName =  "outdoor"
        def controlValue = "TRUE"
        //override def getCommandAfterSequence = ",\"CONTROL:outdoor\",\"TRUE\""
    }
    
    case class InDoorConfigCommand() extends ControlConfigCommand {
        def controlName =  "outdoor"
        def controlValue = "FALSE"
        //override def getCommandAfterSequence = ",\"CONTROL:outdoor\",\"FALSE\""
    }
    
    case class OutDoorHullConfigCommand() extends ControlConfigCommand {
        def controlName =  "flight_without_shell"
        def controlValue = "TRUE"
        //override def getCommandAfterSequence = ",\"CONTROL:flight_without_shell\",\"TRUE\""
    }
    
    case class InDoorHullConfigCommand() extends ControlConfigCommand {
        def controlName =  "flight_without_shell"
        def controlValue = "FALSE"
        //override def getCommandAfterSequence = ",\"CONTROL:flight_without_shell\",\"FALSE\""
    }
    
    case class NavDataDemoConfigCommand() extends GeneralConfigCommand {
        def controlName =  "navdata_demo"
        def controlValue = "TRUE"
    }
    
    abstract class GeneralVideoConfigCommand() extends ControlConfigCommand {
        override def getCommandAfterSequence =",\"video:" + controlName +"\",\""+controlValue+"\""
    }
    
    abstract class VideoChanelConfigCommand() extends GeneralVideoConfigCommand {
        override def controlName =  "video_channel"
        override def controlValue = getChannel
        
        def getChannel :String
    }
    
    case class HorizontalVideoChannelConfigCommand extends VideoChanelConfigCommand {
        override def getChannel = "0"
    }
    
    case class VerticalVideoChannelConfigCommand extends VideoChanelConfigCommand {
        override def getChannel = "1"
    }
    
    case class UseVLibCodecCommand extends GeneralVideoConfigCommand {
        override def controlName = "video_codec"
        override def controlValue = 0x20.toString 
    }
    case class SetVLibCodecBitRateCommand extends GeneralVideoConfigCommand {
        override def controlName = "bitrate"
        override def controlValue = "20000"
    }
    case class LargeHorizontalSmallVerticalVideoChannelConfigCommand extends VideoChanelConfigCommand {
        override def getChannel = "2"
    }
    
    case class LargeVerticalSmallHorizontalVideoChannelConfigCommand extends VideoChanelConfigCommand {
        override def getChannel = "3"
    }
    
    case class VideoOnConfigCommand() extends GeneralConfigCommand {
        def controlName =  "video_enable"
        def controlValue = "TRUE"
    }
    
    case class VideoOffConfigCommand() extends VideoOnConfigCommand {
        override def controlValue = "FALSE"
    }
}