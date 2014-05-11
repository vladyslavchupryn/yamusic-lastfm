package ua.pp.chuprin.yamusicfm

import javax.imageio.ImageIO
import java.io.{FileInputStream, File}
import java.awt.{Rectangle, Color}
import scala.runtime.RichInt
import java.awt.image.{BufferedImage, Raster}
import de.umass.lastfm._
import scala.util.control.Breaks._
import scala.collection.JavaConversions._
import java.util.Properties
import java.util
import org.mozilla.universalchardet.UniversalDetector
import java.nio.charset.Charset
import java.nio.{ByteBuffer, CharBuffer}

object Application extends App {
  val properties = new Properties
  properties.load(new FileInputStream("download.properties"))

  val downloader = new YaMusicDownloader(properties.getProperty("dir"))

  var success = 0;
  var all = 0;
  breakable {
    val limit = properties.getProperty("album-count").toInt
    println("Downloading library from lastfm ...")
    val tracks = ResponseBuilder.buildCollection(
      Caller.getInstance.call("library.getTracks", properties.getProperty("api-key"),
        "user", properties.getProperty("user"),
        "limit", "999"),classOf[Track])

    println("Downloading tracks ...")
    val downloaded = collection.mutable.Set[String]()
    for(track <- tracks) {
      if(track.getArtist != null && track.getAlbum != null) {
        val path = track.getArtist + " - " + track.getAlbum.trim + ":"
        if(!downloaded.contains(path)) {
          downloaded += path

          println(path)
          try {
            all += 1
            downloader.download(track.getArtist, track.getAlbum.trim)
            success += 1
          } catch {
            case _ => println("\t not found :(")
          }
          if(success == limit)
            break
        }
      }
    }
  }
  println("downloaded : " + success + " of " + all)

  Runtime.getRuntime.exit(0)
}
