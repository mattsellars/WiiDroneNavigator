package net.ardrone.actor.util

import se.scalablesolutions.akka.actor.ActorRef
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.actor.Actor._
import se.scalablesolutions.akka.actor.ActorRegistry
import net.ardrone.actor.DroneMovementActor
import net.ardrone.controller.wii.WiiMoteActor
import net.ardrone.terminal.TerminalActor

object ActorUtils {
    def getWiiActorRef = {
       ActorRegistry.actorsFor(classOf[WiiMoteActor]).head
   }
   
   def getDroneActorRef = {
       ActorRegistry.actorsFor(classOf[DroneMovementActor]).head
   }
   
   def getTerminalActorRef = {
        ActorRegistry.actorsFor(classOf[TerminalActor]).head   
   }
}