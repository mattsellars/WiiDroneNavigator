package net.ardrone.ui

import java.awt.image.BufferedImage
/**
 * Interface for something interested in being notified of a new video image
 * from the AR.Drone
 */
trait VideoImageListener {
    def newVideoImage(image:BufferedImage) :Unit
}