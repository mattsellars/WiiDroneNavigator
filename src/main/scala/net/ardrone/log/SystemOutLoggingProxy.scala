package net.ardrone.log

import org.apache.log4j.Logger
import java.io.PrintStream
import java.util.concurrent.atomic.AtomicBoolean

object SystemOutLoggingProxy {
    val outLogProxy = new LogProxy(System.out, Logger.getLogger("System.out"), false)
    val errLogProxy = new LogProxy(System.out, Logger.getLogger("System.err"), true)
    private var isErrProxySet = new AtomicBoolean(false);
    
    def createSysErrProxy = {
        if (!isErrProxySet.getAndSet(true)) {
//            System.setOut(new PrintStream("") {
//                    override def print(value:String) = outLogProxy.print(value) 
//                    override def println(value:String) = outLogProxy.println(value)
//                    
//                });
            System.setErr(new PrintStream("") {
                    override def print(value:String) :Unit = errLogProxy.print(value)
                    override def println(value:String) :Unit = errLogProxy.println(value)
                });
        }
    }
    
    private[SystemOutLoggingProxy] class LogProxy(val realOut:PrintStream, val logger:Logger, val forError:Boolean) {
        val consumeOutToLog = System.getProperty("sys.output.log.mode", "consume").eq("consume")
        def print(value:String) :Unit = {
            if (!consumeOutToLog) {
                realOut.print(value)
            }
            if (forError) {
               logger.error(value) 
            } else {
                logger.info(value)
            }
        }
        
        def println(value:String) :Unit = {
            realOut.println(value)
            if (forError) {
               logger.error(value) 
            } else {
                logger.info(value)
            }
        }
    }
}