package net.ardrone.controller.wii

import org.apache.log4j.Logger
import com.intel.bluetooth.BlueCoveConfigProperties;
import javax.bluetooth.BluetoothStateException;
import javax.bluetooth.DeviceClass;
import javax.bluetooth.DiscoveryAgent;
import javax.bluetooth.DiscoveryListener;
import javax.bluetooth.LocalDevice;
import javax.bluetooth.RemoteDevice;
import javax.bluetooth.ServiceRecord;
//import javax.swing.event.EventListenerList;
import java.util.{Set, HashSet, Collections, List => JList, LinkedList =>JLinkedList}
import java.io.IOException

import motej._
import motej.event._



trait WiiMoteRegistryListener extends MoteFinderListener with MoteDisconnectedListener[Mote];

/**
 * Singleton to replace MoteFinder because MoteFinder sucks ass... scientifically speaking
 * 
 * This will allow you to connect and disconnect the WiiMotes with out restarting the application.
 * 
 * @author Matt Sellars
 *
 */
object WiiMoteRegistry {
    private val log = Logger.getLogger(WiiMoteRegistry.getClass);
    System.setProperty(BlueCoveConfigProperties.PROPERTY_JSR_82_PSM_MINIMUM_OFF, "true");
    
    private val localDevice = LocalDevice.getLocalDevice();
    private val discoveryAgent = localDevice.getDiscoveryAgent();
    
    //private val listenerList = new EventListenerList;
    private val bluetoothAddressCache :Set[String] = Collections.synchronizedSet(new HashSet[String]());
    private val moteDisconnectedListener = new MoteDisconnectedListenerImpl(bluetoothAddressCache, notifyMoteDisconnected);
    private val deviceDiscoveryListener = new DiscoveryListenerImpl(moteDisconnectedListener, notifyMoteRegistered)
    
    /**
     * external registry event listeners to notify on add remove events
     */
    private val registryEventListeners :Set[WiiMoteRegistryListener] = 
        Collections.synchronizedSet(new HashSet[WiiMoteRegistryListener]())
        
    
    /**
     * Find wiimotes that have not been registered in the bluetoothAddressCache
     */
    def findWiiMote(waitTimeMillis:Long) :Unit = {
        try {
            discoveryAgent.startInquiry(DiscoveryAgent.GIAC, deviceDiscoveryListener)
            Thread.sleep(waitTimeMillis)
            discoveryAgent.cancelInquiry(deviceDiscoveryListener)
        } catch {
            case x:Any => { log.error("Error trying to discover wiimote", x) }
        }
    }
    
    def searchForMote() :Unit = {
        try {
            discoveryAgent.startInquiry(DiscoveryAgent.GIAC, deviceDiscoveryListener)
        } catch {
            case t:Throwable => { log.error("Error trying to discover wiimote", t) }
        }
    }
    
    def stopMoteSearch() :Unit = {
        try {
            discoveryAgent.cancelInquiry(deviceDiscoveryListener)
        } catch {
            case t:Throwable => { log.error("Error trying to end wiimote discovery", t) }
        }
    }
    
    def addRegistryListener(listener:WiiMoteRegistryListener) :Unit = {
        if (!registryEventListeners.contains(listener)) registryEventListeners.add(listener)
    }
    
    def removeRegistryListener(listener:WiiMoteRegistryListener) :Unit = {
        if (registryEventListeners.contains(listener)) registryEventListeners.remove(listener)
    }
    
    private def notifyMoteRegistered(mote:Mote) :Unit = {
        val listenerIter = registryEventListeners.iterator
        while(listenerIter.hasNext) {
            listenerIter.next.moteFound(mote)
        }
    }
    
    private def notifyMoteDisconnected(evt:MoteDisconnectedEvent[Mote]) :Unit = {
        val listenerIter = registryEventListeners.iterator
        while(listenerIter.hasNext) {
            listenerIter.next.moteDisconnected(evt)
        }
    }
    
    private class DiscoveryListenerImpl(val moteDisconnectedListener:MoteDisconnectedListener[Mote], 
                                        val notifyMoteRegistered:(Mote) => Unit) extends DiscoveryListener {
        
        def deviceDiscovered(device:RemoteDevice, clazz:DeviceClass) :Unit = {
            if (log.isInfoEnabled()) {
                try {
                    val messBuilder = new StringBuilder().append("Found device: ")
                        .append(device.getFriendlyName(true)).append(" - ")
                        .append(device.getBluetoothAddress).append(" - ")
                        .append(clazz.getMajorDeviceClass).append(":")
                        .append(clazz.getMinorDeviceClass).append(" - ")
                        .append(clazz.getServiceClasses)
                    log.info(messBuilder.toString)
                } catch {
                    case ioe:IOException => {
                        log.error(ioe.getMessage(), ioe)
                    }
                }
                
                try {
                    val deviceName = device.getFriendlyName(true) 
                    if (null != deviceName && deviceName.startsWith("Nintendo")) {
                        val address = device.getBluetoothAddress();
                        // is this already registered?
                        if (!bluetoothAddressCache.contains(address)) {
                            val connectThread = new Thread("connect: " + address) {
                                override def run() :Unit = {
                                    val mote = new Mote(address)
                                    mote.addMoteDisconnectedListener(moteDisconnectedListener)
                                    notifyMoteRegistered(mote)
                                    bluetoothAddressCache.add(address)
                                }
                            }
                            connectThread.start();
                        } else {
                            log.info("WiiMote for address `"+address+"` is already registered")
                        }
                    }
                }  catch {
                    case ioe:IOException => {
                        log.error(ioe.getMessage(), ioe)
                    }
                }
            }
        }
        
        def inquiryCompleted(discType:Int) :Unit = { }
        def servicesDiscovered(transId:Int, servRecords:Array[ServiceRecord]) :Unit = { }
        def serviceSearchCompleted(arg0:Int, arg1:Int) :Unit = { }
    }
    
    private class MoteDisconnectedListenerImpl(val bluetoothAddressCache :Set[String],
                                               val disconnectCallback:(MoteDisconnectedEvent[Mote]) => Unit) extends MoteDisconnectedListener[Mote] {
        def moteDisconnected(evt:MoteDisconnectedEvent[Mote]) :Unit = {
            val mote = evt.getSource
            if (null != mote) {
                if (bluetoothAddressCache.contains(mote.getBluetoothAddress)) {
                    bluetoothAddressCache.remove(mote.getBluetoothAddress)
                    log.info("Mote disconnected: "+mote.getBluetoothAddress)
                }
                disconnectCallback(evt)
            }
        }
    }
}