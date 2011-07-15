package net.ardrone

import net.ardrone.controller.wii.WiiMoteRegistry
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;

object WiiMoteDiscoverTester {
    def main(args : Array[String]): Unit = {
        //WiiMoteRegistry.findWiiMote(30000L);
        //while(true){ Thread.sleep(100L) }
        val droneAddress = getDroneAddress
        val socket = new DatagramSocket()
        var buffer = Array[Byte](0x01, 0x00, 0x00, 0x00);//"8".getBytes //
        socket.send(new DatagramPacket(buffer, buffer.length, droneAddress, 5555));
    }
    
    def getDroneAddress :InetAddress = {
        val addrBytes = List(new java.lang.Integer(192).byteValue(),
                         new java.lang.Integer(168).byteValue(),
                         new java.lang.Integer(1).byteValue(),
                         new java.lang.Integer(1).byteValue());
        return InetAddress.getByAddress(addrBytes.toArray)
    }
}