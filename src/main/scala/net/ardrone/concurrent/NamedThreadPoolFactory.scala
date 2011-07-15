package net.ardrone.concurrent
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

class NamedThreadPoolFactory(name:String) extends ThreadFactory {
   private val threadCount = new AtomicInteger(0)
   
   def newThread(r:Runnable) :Thread = {
     return new Thread(null, r, getName);
   }
   
   def getName :String = name+"-"+threadCount.getAndIncrement
}