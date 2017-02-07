package org.dbpedia.extraction.mappings

import java.io.Writer
import java.text.DecimalFormat
import java.util.concurrent.atomic.AtomicLong

import org.dbpedia.extraction.transform.Quad
import org.dbpedia.extraction.util.{Language, StringUtils}
import org.dbpedia.extraction.wikiparser.{PageNode, WikiPage, WikiTitle}

import scala.collection.mutable

/**
  * Created by Chile on 11/3/2016.
  */
class ExtractionRecorder[T](logFile: Writer = null, val reportInterval: Int = 100000, val preamble: String = null) {

  def this(er: ExtractionRecorder[T]) = this(er.logWriter, er.reportInterval, er.preamble)

  private var failedPageMap = Map[Language, scala.collection.mutable.Map[(Long, T), Throwable]]()
  private var successfulPagesMap = Map[Language, scala.collection.mutable.Map[Long, WikiTitle]]()

  private val startTime = new AtomicLong()
  private var successfulPageCount = Map[Language,AtomicLong]()

  private var logWriter: Writer = null

  private var defaultLang: Language = Language.English
  private var defaultDataset: String = null

  private var openConnections = 0

  private val decForm = new DecimalFormat("#.##")

  setLogFile(logFile, preamble)

  /**
    * A map for failed pages, which could be used for a better way to record extraction fails than just a simple console output.
    *
    * @return the failed pages (id, title) for every Language
    */
  def listFailedPages: Map[Language, mutable.Map[(Long, T), Throwable]] = failedPageMap

  /**
    * define the log file destination
    *
    * @param logFile the target file
    * @param preamble the optional first line of the log file
    */
  def setLogFile(logFile: Writer, preamble: String = null): Unit ={
    logWriter = logFile
    if(logWriter != null && preamble != null && preamble.length > 0)
      logWriter.append("# " + preamble + "\n")
  }

  /**
    * successful page count
    *
    * @param lang - for this language
    * @return
    */
  def successfulPages(lang: Language): Long = successfulPageCount.get(lang) match{
    case Some(m) => m.get()
    case None => 0
  }

  /**
    * get successful page count after increasing it by one
    *
    * @param lang - for this language
    * @return
    */
  def increaseAndGetSuccessfulPages(lang: Language): Long ={
    successfulPageCount.get(lang) match {
      case Some(ai) => ai.incrementAndGet()
      case None => {
        successfulPageCount += (lang -> new AtomicLong(1))
        1
      }
    }
  }

  /**
    * number of failed pages
    *
    * @param lang - for this language
    * @return
    */
  def failedPages(lang: Language): Long = failedPageMap.get(lang) match{
    case Some(m) => m.size
    case None => 0
  }

  /**
    * the current accumulated page number
    *
    * @param lang - for this language
    * @return
    */
  def runningPageNumber(lang:Language) = successfulPages(lang) + failedPages(lang)

  /**
    * prints a message of a RecordEntry if available and
    * assesses a RecordEntry for the existence of a Throwable and forwards
    * the record to the suitable method for a failed or successful extraction
    *
    * @param records - the RecordEntries for a WikiPage
    */
  def record(records: RecordEntry[T]*): Unit = {
    for(record <- records) {
      //val count = increaseAndGetSuccessfulPages(record.language)
      record.page match{
        case page: WikiPage => {
          if (record.errorMsg != null)
            printLabeledLine(record.errorMsg, record.severity, page.title.language, Seq(PrinterDestination.err, PrinterDestination.file))
          Option(record.error) match {
            case Some(ex) => failedRecord(page.title.encoded, page.id, record.page, ex, record.language)
            case None => recordExtractedPage(page.id, page.title, record.logSuccessfulPage)
          }
        }
        case quad: Quad =>{
          Option(record.error) match {
            case Some(ex) => failedRecord(quad.subject, runningPageNumber(record.language), record.page, ex, record.language)
            case None => recordQuad(quad, record.severity, record.language)
          }
        }
        case _  => {
          val msg = Option(record.errorMsg) match{
            case Some(m) => m
            case None => {
              if(record.error != null) record.error.getMessage
              else "an undefined error occurred at quad: " + successfulPages(record.language)
            }
          }
          printLabeledLine(msg, record.severity, record.language, null)
        }
      }
    }
  }

  /**
    * adds a new fail record for a wikipage which failed to extract; Optional: write fail to log file (if this has been set before)
    *
    * @param id - page id
    * @param node - PageNode of page
    * @param exception  - the Throwable responsible for the fail
    */
  def failedRecord(name: String, id: Long, node: T, exception: Throwable, language:Language = null): Unit = synchronized{
    val lang = if(language != null) language else defaultLang
    val tag = node match{
      case p: PageNode => "page"
      case q: Quad => "quad"
      case _ => "instance"
    }
    failedPageMap.get(lang) match{
      case Some(map) => map += ((id,node) -> exception)
      case None =>  failedPageMap += lang -> mutable.Map[(Long, T), Throwable]((id, node) -> exception)
    }
    printLabeledLine("extraction failed for " + tag + " " + id + ": " + name + ": " + exception.getMessage(), RecordSeverity.Exception, lang, Seq(PrinterDestination.file))
    for (ste <- exception.getStackTrace)
      printLabeledLine("\t" + ste.toString, RecordSeverity.Exception, lang, Seq(PrinterDestination.file), noLabel = true)
    printLabeledLine("extraction failed for " + tag + " " + id + ": " + name, RecordSeverity.Exception, lang, Seq(PrinterDestination.err, PrinterDestination.file))
  }

  /**
    * adds a record of a successfully extracted page
    *
    * @param id - page id
    * @param title - page title
    * @param logSuccessfulPage - indicates whether the event of a successful extraction shall be included in the log file (default = false)
    */
  def recordExtractedPage(id: Long, title: WikiTitle, logSuccessfulPage:Boolean = false): Unit = synchronized {
    if(logSuccessfulPage) {
      successfulPagesMap.get(title.language) match {
        case Some(map) => map += (id -> title)
        case None => successfulPagesMap += title.language -> mutable.Map[Long, WikiTitle](id -> title)
      }
      printLabeledLine("page " + id + ": " + title.encoded + " extracted", RecordSeverity.Info, title.language, Seq(PrinterDestination.file))
    }
    if(increaseAndGetSuccessfulPages(title.language) % reportInterval == 0)
      printLabeledLine("extracted {page} pages; {mspp} per page; {fail} failed pages", RecordSeverity.Info, title.language)
  }

  /**
    * record (successful) quad
    *
    * @param quad
    * @param lang
    */
  def recordQuad(quad: Quad, severity: RecordSeverity.Value, lang:Language): Unit = synchronized {
    if(increaseAndGetSuccessfulPages(lang) % reportInterval == 0)
      printLabeledLine("processed {page} quads; {mspp} per quad; {fail} failed quads", severity, lang)
  }

    /**
    * print a line to std out, err or the log file
    *
    * @param line - the line in question
    * @param language - langauge of current page
    * @param print - enum values for printer destinations (err, out, file - null mean all of them)
    * @param noLabel - the initial label (lang: time passed) is omitted
    */
  def printLabeledLine(line:String, severity: RecordSeverity.Value, language: Language = null, print: Seq[PrinterDestination.Value] = null, noLabel: Boolean = false): Unit ={
    val lang = if(language != null) language else defaultLang
    val printOptions = if(print == null) {
      if(severity == RecordSeverity.Exception )
        Seq(PrinterDestination.err, PrinterDestination.out, PrinterDestination.file)
      else if(severity == RecordSeverity.Info)
        Seq(PrinterDestination.out, PrinterDestination.file)
      else
        Seq(PrinterDestination.file)
    } else print

    val pages = successfulPages(lang)
    val time = System.currentTimeMillis - startTime.get
    val replacedLine = (if (noLabel) "" else severity.toString + "; " + lang.wikiCode + "; extraction at {time}{data}; ") + line
    val pattern = "\\{\\s*\\w+\\s*\\}".r
    var lastend = 0
    var resultString = ""
    for(matchh <- pattern.findAllMatchIn(replacedLine)){
      resultString += replacedLine.substring(lastend, matchh.start)
      resultString += (Option(matchh.matched) match{
        case Some(m) =>
          m match{
            case i if i == "{time}" => StringUtils.prettyMillis(time)
            case i if i == "{mspp}" => decForm.format(time.toDouble / pages) + " ms"
            case i if i == "{page}" => pages.toString
            case i if i == "{fail}" => failedPages(lang).toString
            case i if i == "{data}" => if(defaultDataset != null) " for dataset " + defaultDataset else ""
            case _ => ""
          }
        case None => ""
      })
      lastend = matchh.end
    }
    resultString += replacedLine.substring(lastend)

    for(pr <-printOptions)
      pr match{
        case PrinterDestination.err => System.err.println(resultString)
        case PrinterDestination.out => System.out.println(resultString)
        case PrinterDestination.file if logWriter != null => logWriter.append(resultString + "\n")
        case _ =>
      }
  }

  def initialize(lang: Language, dataset: String = null): Unit ={
    startTime.set(System.currentTimeMillis)
    defaultLang = lang
    defaultDataset = dataset
    openConnections += 1
  }

  override def finalize(): Unit ={
    openConnections -= 1
    if(openConnections == 0 && logWriter != null)
      logWriter.close()
    logWriter = null
    super.finalize()
  }

  def resetFailedPages(lang: Language) = failedPageMap.get(lang) match{
    case Some(m) => {
      m.clear()
      successfulPageCount(lang).set(0)
    }
    case None =>
  }

  object PrinterDestination extends Enumeration {
    val out, err, file = Value
  }

}

/**
  * This class provides the necessary attributes to record either a successful or failed extraction
  *
  * @param page
  * @param language
  * @param errorMsg
  * @param error
  * @param logSuccessfulPage
  */
class RecordEntry[T](
  val page: T,
  val severity: RecordSeverity.Value,
  val language: Language,
  val errorMsg: String= null,
  val error:Throwable = null,
  val logSuccessfulPage:Boolean = false
)

object RecordSeverity extends Enumeration {
  val Info, Warning, Exception = Value
}