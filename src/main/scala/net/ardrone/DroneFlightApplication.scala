package net.ardrone

import net.ardrone.terminal.TerminalActor
import se.scalablesolutions.akka.actor.Actor._
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.config.Supervision._
import net.ardrone.actor.DroneMovementActor
import net.ardrone.controller.wii.WiiMoteActor
import net.ardrone.model.TerminalCommands
import net.ardrone.model.WiiMoteCommands

object DroneFlightApplication {

    def main(args : Array[String]): Unit = {
        val terminalActor = actorOf[TerminalActor]
        val droneActor = actorOf[DroneMovementActor]
        val wiiMoteActor = actorOf[WiiMoteActor]
        
        /*
         * Supervisor to restart actors if any die
         */
        val supervisor = Supervisor(
                SupervisorConfig(
                        AllForOneStrategy(List(classOf[Exception]), 3, 1000),
                        Supervise(droneActor, Permanent) ::
                        Supervise(terminalActor, Permanent) ::
                        Supervise(wiiMoteActor, Permanent) :: Nil))
        val future = wiiMoteActor !!! WiiMoteCommands.StartBlueTooth
        future.await.result //let bluetooth setup before app starts
        
        //startup terminal input
        terminalActor ! new TerminalCommands.InitializeCommand(droneActor, wiiMoteActor)
    }
}