package net.ardrone.ui
import se.scalablesolutions.akka.actor.ActorRef
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.actor.Actor._

import java.lang.{Integer => JInt}
import java.util.{Observable, Queue, Observer, Hashtable}
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.AtomicReference
import java.awt.image.BufferedImage
import java.awt.{Graphics, Graphics2D, Font, GridBagLayout, GridBagConstraints, Toolkit}
import javax.swing.{JFrame, JPanel, JToolBar, JButton, BorderFactory, JSlider, SwingConstants, JComponent, JLabel, BoxLayout, Box}
import org.apache.log4j.Logger
import java.awt.{BorderLayout, Color, Component, Rectangle, Dimension}
import java.awt.event.{ActionListener, ActionEvent}
import javax.swing.event.{ChangeListener, ChangeEvent}
import net.ardrone.actor.util.ActorUtils._
import net.ardrone.model.DroneCommands._
import java.awt.geom.AffineTransform
import java.awt.image.RescaleOp

object VideoFrameDisplayer {
    private val logger :Logger = Logger.getLogger(VideoFrameDisplayer.getClass)
    
    private val imagesToRenderQueue = new ArrayBlockingQueue[BufferedImage](100);
    private val imageFetcher = new ImageQueueProducer(imagesToRenderQueue)
    private val renderer:VideoRenderer = new VideoRenderer
    
    private val horizontalChannelActionListener:ActionListener = new ActionListener() {
        def actionPerformed(evt:ActionEvent) :Unit = {
            getDroneActorRef ! DRONE_SET_VIDEO_CHANNEL_HZ_ONLY
        }
    }
    
    private val verticalChannelActionListener:ActionListener = new ActionListener() {
        def actionPerformed(evt:ActionEvent) :Unit = {
            getDroneActorRef ! DRONE_SET_VIDEO_CHANNEL_VRT_ONLY
        }
    }
    
    private val horizontalAndVertChannelActionListener:ActionListener = new ActionListener() {
        def actionPerformed(evt:ActionEvent) :Unit = {
            getDroneActorRef ! DRONE_SET_VIDEO_CHANNEL_HZ_SMVRT
        }
    }
    
    private val vertAndHorizontalChannelActionListener:ActionListener = new ActionListener() {
        def actionPerformed(evt:ActionEvent) :Unit = {
            getDroneActorRef ! DRONE_SET_VIDEO_CHANNEL_VRT_SMHZ
        }
    }
    
    private val brightnessChangeListener = new ChangeListener() {
        def stateChanged(evt:ChangeEvent) {
            if (null != evt) {
                if (evt.getSource.isInstanceOf[JSlider]) {
                    val slider = evt.getSource.asInstanceOf[JSlider]
                    val sliderVal = slider.getValue.toFloat / 100f
                    renderer.setBrightnessFactor(1f + sliderVal)
                }
            }
        }
    }
    
    renderer.setAlignmentX(Component.CENTER_ALIGNMENT);
    renderer.setAlignmentY(Component.CENTER_ALIGNMENT);
    renderer.setDoubleBuffered(true)
    
    imageFetcher.addObserver(renderer)

    private val mainFrame = new JFrame
    mainFrame.setLayout(new BorderLayout)
    mainFrame.setTitle("Drone Video Feed")
    val toolbarPanel = createVideoChannelToolBar
    toolbarPanel.add(createBrightnessSlider(brightnessChangeListener), BorderLayout.EAST)
  
    mainFrame.add(toolbarPanel, BorderLayout.NORTH)
    //mainFrame.add(renderer, BorderLayout.CENTER)
    mainFrame.add(createVideoPanel(renderer), BorderLayout.CENTER)
    mainFrame.pack()
    mainFrame.setMinimumSize(new Dimension(450,450))
    mainFrame.setSize(450,450)
    
    def showVideo {
        val screenDimension = Toolkit.getDefaultToolkit().getScreenSize();
        val x = (screenDimension.width-mainFrame.getSize().width)/2;
        val y = (screenDimension.height-mainFrame.getSize().height)/2;
        mainFrame.setLocation(x, y);
        mainFrame.setVisible(true);
    }
    
    def hideVideo {
        mainFrame.setVisible(false);
    }
    
    def getVideoConsumer :VideoImageListener = imageFetcher
 
    private def createVideoPanel(renderer:VideoRenderer) :JPanel = {
        val panel = new JPanel
        panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS))
        val blackline = BorderFactory.createLineBorder(Color.black);
        val titledBorder = BorderFactory.createTitledBorder(blackline, "Drone Video")
        panel.setBorder(titledBorder)
        panel.add(renderer, BorderLayout.CENTER)
        return panel
    }
    
    private def createBrightnessSlider(listener:ChangeListener) :JPanel = {
        val min = 0
        val max = 200
        val panel = new JPanel
        panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS)) //vertical layout
        val sliderLabel = new JLabel("Brightness", SwingConstants.CENTER);
        sliderLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        panel.add(sliderLabel)
        val slider:JSlider = new JSlider(SwingConstants.HORIZONTAL, min, max, 100/*INITIAL SLIDER VAL*/)
        val labelTable = new Hashtable[JInt, JComponent]()
        labelTable.put(min, new JLabel("Darker"))
        labelTable.put(max, new JLabel("Lighter"))
        slider.setLabelTable(labelTable)
        slider.setPaintTicks(true)
        slider.setPaintLabels(true)
        slider.setMajorTickSpacing(50)
        slider.setMinorTickSpacing(25)
        val font = slider.getFont.deriveFont(Font.BOLD, 12f);
        slider.setFont(font);
        sliderLabel.setFont(font)
        slider.addChangeListener(listener)
        panel.add(slider)
        return panel
    }
    
    private def createVideoChannelToolBar :JPanel = {
        val panel = new JPanel
        val blackline = BorderFactory.createLineBorder(Color.black);
        val titledBorder = BorderFactory.createTitledBorder(blackline, "Video Channels")
        panel.setLayout(new BorderLayout)
        panel.setBorder(titledBorder)
        
        val toolbar:JToolBar = new JToolBar
        toolbar.setFloatable(false)
        var button:JButton = new JButton("Video 1")
        button.addActionListener(horizontalChannelActionListener)
        toolbar.add(button)
        toolbar.addSeparator
        button = new JButton("Video 2")
        button.addActionListener(verticalChannelActionListener)
        toolbar.add(button)
        toolbar.addSeparator
        button = new JButton("Video 3")
        button.addActionListener(horizontalAndVertChannelActionListener)
        toolbar.add(button)
        toolbar.addSeparator
        button = new JButton("Video 4")
        button.addActionListener(vertAndHorizontalChannelActionListener)
        toolbar.add(button)
        
        panel.add(toolbar, BorderLayout.WEST)
        return panel
    }
    
    protected class VideoRenderer extends JPanel with Observer {
        private val log = Logger.getLogger(this.getClass)
        private val videoWidth:Int = 400
        private val videoHeight:Int = 300
        private val brightnessFactor = new AtomicReference[RescaleOp](new RescaleOp(1.5f, 0, null))
        setMinimumSize(new Dimension(400, 300))
        setMaximumSize(new Dimension(400, 300))
        setAlignmentX(SwingConstants.CENTER)
        
        def setBrightnessFactor(factor:Float) :Unit = {
            log.info("Attempted Brightness Set: "+factor)
            if (factor >= 1f && factor <= 3f) {
                brightnessFactor.set(new RescaleOp(factor, 0, null))
            } else if (log.isDebugEnabled) {
                log.debug("Brightness Factor of "+factor+" not supported");
            }
        }
        
        override def paintComponent(graphics:Graphics) {
            if (graphics.isInstanceOf[Graphics2D]) {
                val g2d:Graphics2D = graphics.asInstanceOf[Graphics2D];
                val image:BufferedImage = imagesToRenderQueue.poll
                var xScale = 1f
                var yScale = 1f
                if (null != image) {
                    if (videoHeight != image.getHeight) {
                        yScale = videoHeight.toFloat / image.getHeight.toFloat
                    }
                    if (videoWidth != image.getWidth) {
                        xScale = videoWidth.toFloat / image.getWidth.toFloat
                    }
                    g2d.drawImage(brightnessFactor.get.filter(image, null), 
                                      AffineTransform.getScaleInstance(xScale, yScale), null);//, 0, 0)
                    g2d.dispose();
                }
            }
        }
        
       override def update(observable:Observable, arg:Object) :Unit = {
           if (observable.isInstanceOf[ImageQueueProducer]) {
               
               if (logger.isDebugEnabled) logger.debug("Received image render update")
               
               if (null != getGraphics) {
                   paint(getGraphics)
               } else {
                   log.warn("AWT Graphics are null, unable to render video")
               }
           }
       }
    }
}

class ImageQueueProducer(imagesToRenderQueue:Queue[BufferedImage]) extends Observable with VideoImageListener {
    def newVideoImage(image:BufferedImage) :Unit = {
        imagesToRenderQueue.offer(image)
        setChanged
        notifyObservers
    }
}