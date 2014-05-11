package ua.pp.chuprin.yamusicfm

import dispatch._
import Defaults._
import java.security.MessageDigest
import org.apache.commons.codec.binary.Hex
import scala.xml._
import scala.xml.parsing.{XhtmlParser, NoBindingFactoryAdapter}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import java.io.{RandomAccessFile, File}
import java.net.URL
import sys.process._
import com.mpatric.mp3agic.{ID3v1Tag, Mp3File}

class YaMusicDownloader(val destination : String) {

  def download(artist : String, album : String) = {
    val albumDirectory = new File(destination + "/" + artist + "/" + album)
    if(albumDirectory.exists()) {
      println("\t already exists :)")
    } else {
      val artists = html(s"http://music.yandex.ru/fragment/search?text=$artist&type=artist&page=0")
      val artistRef = ((artists \\ "div" filter((node) => {
        (node \ "@class").text.contains("b-artist-group")
      }))(0) \ "a" \ "@href")(0).text
      val artistId = readId(artistRef)

      val albums = html(s"http://music.yandex.ru/fragment/artist/$artistId/albums")
      val albumRef = ((albums \\ "a" filter ((node) => {
        (node \ "@class").text.contains("b-link_class_albums-title-link") && node.text.toLowerCase == album.toLowerCase
      })) \ "@href")(0).text
      val albumId = readId(albumRef)

      val tracks = html(s"http://music.yandex.ru/fragment/album/$albumId")
      (tracks \\ "div" filter ((node) => {
        (node \ "@class").text.contains("b-track ")
      })).map((node) => {
        (node \ "@onclick").text
      }).foreach((onclick) => {
        val jsonAst = parse(onclick.substring("return ".length))

        implicit val formats = DefaultFormats
        val track = trackUrl((jsonAst \ "id").extract[String], (jsonAst \ "storage_dir").extract[String])

        val name = (jsonAst \ "title").extract[String]
        println("\t" + name)
        albumDirectory.mkdirs()
        val file = new File(albumDirectory, name + ".mp3")
        new URL(track) #> file !!

        val id3v1Tag = new ID3v1Tag
        id3v1Tag.setArtist(artist);
        id3v1Tag.setTitle(name);
        id3v1Tag.setAlbum(album);

        val saveFile = new RandomAccessFile(file, "rw")
        saveFile.seek(saveFile.length)
        saveFile.write(id3v1Tag.toBytes)
        saveFile.close
      })
    }
  }

  def trackUrl(id : String, storageDir : String) = {
    val infoPathSoup = XML.loadString(
      Http(url(s"http://storage.music.yandex.ru/get/$storageDir/2.xml") OK as.String).apply())

    val fileName = (infoPathSoup \ "@filename")(0).text
    val fileData = XML.loadString(
      Http(url(s"http://storage.music.yandex.ru/download-info/$storageDir/$fileName") OK as.String).apply())

    val host = (fileData \ "host")(0).text
    val path = (fileData \ "path")(0).text
    val key = get_key(path, (fileData \ "s")(0).text)
    val ts = (fileData \ "ts")(0).text

    s"http://$host/get-mp3/$key/$ts$path?track-id=$id&region=225&from=service-search"
  }

  def get_key(path : String, s : String) = {
    val key = path.substring(1) + s
    md5("XGRlBW9FXlekgbPrRHuSiA" + key.replaceAll("\r\n", "\n"))
  }

  def md5(s: String) = {
    Hex.encodeHexString(MessageDigest.getInstance("MD5").digest(s.getBytes))
  }

  def html(url : String) = {
    val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
    val parser = parserFactory.newSAXParser()
    val source = new org.xml.sax.InputSource(url)
    val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
    adapter.loadXML(source, parser)
  }

  def readId(link : String) = {
    link.substring(link.lastIndexOf('/') + 1).toInt
  }
}
