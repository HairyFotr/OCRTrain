import sys.process._
import util.Random._
import collection.mutable.{Buffer,HashSet,ListBuffer}
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.util._
import scala.concurrent.duration._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.annotation.tailrec
import math._
import Utils._

object OCRTrain extends App {
  //setSeed(0)
  type Params = List[String]
  def emptyParams: Params = List()

  // Split ImageMagick parameters
  def splitParams(params: Params): Params = splitParams(params.mkString(" "))
  def splitParams(params: String): Params = {
    params.replaceAll(", ", " ").split(" ")
    .map(_.trim)
    .filter(_.size > 0)
    .foldLeft(emptyParams)((acc, str) => 
      if((str.size > 1 && str(1).isDigit) || (str(0) != '-' && str(0) != '+'))
        acc.init :+ (acc.last + " " + str) 
      else
        acc :+ str)
    .filter(_.size > 0)
  }

  val allowedSingleLetters = "[aeiouy]" // Y U NO allow other letters :)
  val allowedChars = "a-zA-Z\\s"//"A-Z0-9"//
  val gocrAllowedChars = allowedChars.replace("\\s":CharSequence, "":CharSequence)
  val ocradAllowedChars = 
    (allowedChars.contains("a-z") || allowedChars.contains("A-Z"), allowedChars.contains("0-9")) match {
      case (false, false) => ""
      case (true, false) => "--filter=letters"
      case (false, true) => "--filter=numbers"
      case (true, true) => ""
    }
  
  def stringFilter(str: String): Array[String] = 
    str.toLowerCase
      .replaceAll("[,.!?-_:;]", " ")
      .replaceAll("[^"+allowedChars.toLowerCase+"]", "")
      .replaceAll("\\s+", " ")
      .split(" ")
      .map(_.trim)
      .filter(word => word.size > 1 || word.matches(allowedSingleLetters))

  val images = 
    fromFile(args(0))
      .map(_.trim)
      .filterNot(line => line.isEmpty || line(0) == '#')
      .map(_.split(" "))
      .map(line => (line.head, line.tail.flatMap(stringFilter).mkString(" ")))
      .par
    
  //images.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(6))

  def OCR(file: String, engine: Int, params: Params): String = {
    try {
      Await.result(future {
        (s"""convert ram/img/$file -compress none ${params.mkString(" ")} ram/$file"""+(if(engine == 3 || engine == 6)".pnm"else"")).!!
        engine match {
          case 0 => (s"""tesseract ram/$file ram/$file""").!!
          case 1 => (s"""gocr -C $gocrAllowedChars -i ram/$file -o ram/$file.txt""").!!
          case 2 => (s"""cuneiform ram/$file -o ram/$file.txt""").!!
          case 3 => (s"""ocrad -lf $ocradAllowedChars --format=utf8 -o ram/$file.txt ram/$file.pnm""").!!
          case 4 => (s"""cuneiform --dotmatrix ram/$file -o ram/$file.txt""").!!
          case 5 => (s"""cuneiform --fax ram/$file -o ram/$file.txt""").!!
          case 6 => (s"""ocrad -f $ocradAllowedChars --format=utf8 -o ram/$file.txt ram/$file.pnm""").!!
          case 7 => (s"""gocr -m 4 -C $gocrAllowedChars -i ram/$file -o ram/$file.txt""").!!
          case 8 => (s"""gocr -m 16 -C $gocrAllowedChars -i ram/$file -o ram/$file.txt""").!!
          case 9 => (s"""gocr -m 32 -C $gocrAllowedChars -i ram/$file -o ram/$file.txt""").!!
          case 10 => (s"""gocr -i ram/$file -o ram/$file.txt""").!!
        }
      }, 20.seconds)
      
      val out = stringFilter(fromFile(s"ram/$file.txt").mkString(" "))
      out.mkString(" ")
    } catch {
      case e: Exception => 
        println(s"EXCEPTION in $file ... $e")
        ""
    }
  }
    
  def postprocess(engine: Int) {
    try {
      ("pkill convert").!
      engine match {
        case 0 => ("pkill tesseract").!
        case 1 => ("pkill gocr").!
        case 2|4|5 => ("pkill cuneiform").!
        case 3|6 => ("pkill ocrad").!
        case _ => ("pkill gocr").!
      }
      Seq("bash", "-c", "rm ram/*").!!
      ()
    } catch { 
      case e: Exception => 
    }
  }

  // http://www.imagemagick.org/Usage/morphology/#alturnative
  val morphology1 = List("Dilate","Smooth","Erode","Open","Close","Edge","EdgeIn","EdgeOut", "Convolve") ++ List("Erode", "Dilate", "Open", "Close").map(_ + "I")
  val morphology2 = List("Diamond","Square", "Manhattan", "Chebyshev", "Euclidean", "Sobel")
  val interpolate = List("NearestNeighbor","Average","Average4","Average9","Bilinear")
  val statistics = List("Gradient","Maximum","Mean","Median","Minimum","Mode","Nonpeak","StandardDeviation")
  
  def randomParam: String = {
    def nextDoubleStr(x: Double, y: Double = 0): String = (nextFloat*x + y).toString.take(4)
    //TODO: optimize, now that it's only one param :)
    def params: Params = List(
      "-brightness-contrast "+(nextInt(101)-50)+"x"+(nextInt(101)-50),
      "-brightness-contrast "+(nextInt(201)-100)+"x0",
      "-brightness-contrast 0x"+(nextInt(201)-100),
      "-threshold "+nextInt(101)+"%",
      "-black-threshold "+nextInt(101)+"%",
      "-white-threshold "+nextInt(101)+"%",
      "-solarize "+nextInt(101)+"%",
      "-gamma "+nextDoubleStr(1, -0.5),
      "-level "+nextInt(101)+"%x"+nextInt(101)+"%",
      "-level-colors grey"+nextInt(101)+",grey"+nextInt(101),
      "-background gray"+nextInt(101),
      "-fill gray"+nextInt(101),
      //"-opaque gray"+nextInt(101),//-fill required
      "-morphology "+morphology1.random+" "+morphology2.random+":"+(nextInt(4)+1),
      "-morphology Convolve Sobel:"+nextInt(360),
      "-interpolate "+interpolate.random,
      "-blur "+nextInt(5)+"x"+nextDoubleStr(4),
      "-sharpen "+nextInt(5)+"x"+nextDoubleStr(4),
      "-adaptive-blur "+nextInt(5)+"x"+nextDoubleStr(4),
      "-adaptive-sharpen "+nextInt(5)+"x"+nextDoubleStr(4),
      "-unsharp "+nextInt(5)+"x"+nextInt(5)+"x"+nextDoubleStr(4),
      "-fuzz "+(nextInt(75)+1)+"% -trim",
      "-fill grey"+nextInt(101)+" "+"-fuzz "+(nextInt(75)+1)+"% "+"-floodfill "+(nextInt(5)*25)+"%x"+(nextInt(5)*25)+"% grey"+nextInt(101),
      "-sigmoidal-contrast "+(nextInt(20)+1)+"x"+(nextInt(100)+1)+"%",
      "+sigmoidal-contrast "+(nextInt(20)+1)+"x"+(nextInt(100)+1)+"%",
      "-contrast-stretch "+nextInt(101)+"%x"+nextInt(101)+"%",
      "-virtual-pixel Edge +distort SRT "+nextDoubleStr(10,-5),
      "-deskew "+(nextInt(100)+1)+"%",
      (if(nextBoolean)"+"else"-")+"contrast",
      "-colorspace Gray",
      "-liquid-rescale "+(nextInt(31)+80)+"%x100%",
      "-liquid-rescale 100%x"+(nextInt(31)+80)+"%",
      "-liquid-rescale "+(nextInt(31)+80)+"%x"+(nextInt(31)+80)+"%",
      "-scale "+(nextInt(41)+80)+"%x"+(nextInt(41)+80)+"%",
      "-gravity center -extent "+(nextInt(21)+90)+"%x"+(nextInt(21)+90)+"%",
      "-wave "+(nextInt(10)+2)+"x"+(nextInt(800)+10),
      "-implode "+(nextFloat-0.5).toString.take(4),
      "-swirl "+(nextFloat-0.5).toString.take(4),
      "-shear "+((nextFloat-0.5)*5).toString.take(4)+"x"+((nextFloat-0.5)*5).toString.take(4),
      "-noise "+nextInt(10)+"x"+nextInt(10),
      "-spread "+nextInt(5),
      "-statistic "+statistics.random+" "+nextInt(7)+"x"+nextInt(7))
    
    params.random
  }
  def randomParams(n: Int): Params = (0 until n).map(i => randomParam).toList
  
  val nextParams = fromFile(if(args.size >= 2) args(1) else "params")
  def randomNextParam: String = nextParams.random
  def randomNextParams(n: Int): Params = (0 until n).map(i => randomNextParam).toList

  for(engine <- 0 to 10) {// ++ (4 to 10)) {
    object ResultCache {
      val oldResults = HashSet[String]()
      // Check if we already ran with these params, or save
      def newResult(phaseResult: Params): Boolean = {
        if(phaseResult == null) {
          false
        } else {
          val result = phaseResult.mkString(" ")
          if(oldResults contains result) false else { oldResults += result; true }
        }
      }
    }
    import ResultCache.newResult
    
    abstract class Phase(
        val name: String = "Phase", 
        var stepLimit: Int = 1, 
        var failLimit: Int = 1,
        var enabledAtReset: Boolean = true) {

      var enabled = enabledAtReset
      var step = 0
      var fail = 0
      def reset() {
        enabled = enabledAtReset
        step = 0
        fail = 0
      }

      def precondition(params: Params): Boolean = true

      // returns: Result and Summary
      type Result = Option[(Params, String)]
      def applyPhase(params: Params): Result

      def apply(paramLists: Params*): Result = {
        if(enabled == false || (stepLimit > 0 && step >= stepLimit) || (failLimit > 0 && fail >= failLimit)) {
          enabled = false
          None
        } else {
          println(name+" "+step+":"+stepLimit+" "+fail+":"+failLimit+" "+paramLists+" ")
          val goodParamLists = paramLists.filter(precondition)
          if(goodParamLists.isEmpty) {
            step += 1
            fail += 1
            None
          } else {
            applyPhase(goodParamLists.random) match {
              case Some(result) if(newResult(result._1)) => 
                step += 1
                Some((result._1, name + result._2))
              case _ => 
                step += 1
                fail += 1
                None
            }
          }
        }
      }
    }

    // Start with these params
    val preloads: List[Params] = (
      List(List()) ++ 
      fromFile("preload").map(_.trim)
        .filterNot(line => line.isEmpty || line(0) == '#')
        .dropWhile(line => line != "---").drop(1)
        .takeWhile(line => line != "---")
        .distinct
        .map(splitParams).toList)
    
    object Preload extends Phase(name = "Preload", stepLimit = preloads.size, failLimit = -1) {
      override def precondition(params: Params): Boolean = step < preloads.size
      override def applyPhase(params: Params): Result = Option { (preloads(step), step.toString) }
    }
    object Insert extends Phase(name = "Insert", stepLimit = 50, failLimit = -1) {
      override def applyPhase(params: Params): Result = Option {
        val howMany = 1 + nextInt(3)
        
        var out = params
        val poss = ListBuffer[Int]()
        for(i <- 0 until howMany) {
          var pos = if(out.isEmpty) 0 else if(nextBoolean) nextInt(out.size) else out.size-1
          poss += pos
          out = insert(out, pos, if(nextBoolean) randomNextParam else randomParam)
        }
        
        (out, poss.mkString(","))
      }
    }
    object Trim extends Phase(name = "Trim", stepLimit = -1, failLimit = 30) {
      override def precondition(params: Params): Boolean = params.size match {
        case 2 => step < params.size
        case 3 => step < params.size * 2
        case x if(x > 3) => step < params.size * 3
        case _ => false
      }
      override def applyPhase(params: Params): Result = Option {
        step/params.size match {
          case 0 if(params.size > 2) => // random 2
            var out = params
            var (i,j) = (nextInt(params.size), nextInt(params.size-1))
            if(i == j || (j == 0 && i == params.size-1)) i = nextInt(params.size) // prevent at least some cases of repeating "case 1"
            out = drop(out, i, 1)
            out = drop(out, j, 1)
            (out, "r2")
          case 1 => // every 2 neighbours
            var out = params
            if(step%params.size == params.size-1) { // on last step drop first and last
              out = drop(out, step%params.size, 1)
              out = drop(out, 0, 1)
            } else {
              out = drop(out, step%params.size, 2)
            }
            (out, "2")
          case 0|2 => // every single param
            var out = params
            out = drop(out, step, 1)
            (out, "1")
          case _ => 
            null
        }
      }
    }
    object Swap extends Phase(name = "Swap", stepLimit = -1, failLimit = 30) {
      override def precondition(params: Params): Boolean = (params.size match {
        case 2 => step < params.size
        case x if(x > 2) => step < params.size * 2
        case _ => false
      }) || (!enabled)
      override def applyPhase(params: Params): Result = Option {
        step/params.size match {
          case 1 => // swap neighbours
            var out = params
            var (i,j) = ((step)%params.size, (step+1)%params.size)
            out = swap(out, i, j)
            (out, s"$i,$j")
          case _ if params.size > 2 => // random swap
            var out = params
            var i,j = nextInt(params.size)
            if(abs(j-i) <= 1) i = nextInt(params.size) // maybe prevent doing "case 1" and swapping with self
            out = swap(out, i, j)
            (out, s"r$i,$j")
          case _ =>
            null
        }
      }
    }
    object Mutate extends Phase(name = "Mutate", stepLimit = 150, failLimit = -1) {
      override def precondition(params: Params): Boolean = params.mkString matches ".*[0-9].*"
      override def applyPhase(params: Params): Result = mutate(params, step%2 + 1, step%3 + 1)

      private val opReg = "([-+ a-zA-Z]+?[ :y])" // space or morphology Disk:1 or grey0
      private val percentXpercentColor = s"$opReg([0-9]+)%x([0-9]+)(% grey)([0-9]+)".r //floodfill
      private val colorColor      = s"$opReg(grey)([0-9]+)(,grey)([0-9]+)".r
      private val percentXpercent = s"$opReg([0-9]+)%x([0-9]+)(%.*)".r
      private val percent         = s"$opReg([0-9]+)(%.*)".r
      private val numXpercent     = s"$opReg([+-]?[0-9]+)x([0-9]+)(%.*)".r
      private val floatXfloat     = s"$opReg([+-]?[0-9]+[.][0-9]+)x([+-]?[0-9]+[.][0-9]+)(.*)".r
      private val numXfloat       = s"$opReg([+-]?[0-9]+)x([+-]?[0-9]+[.][0-9]+)(.*)".r
      private val numXnum         = s"$opReg([+-]?[0-9]+)x([+-]?[0-9]+)(.*)".r
      private val float           = s"$opReg([+-]?[0-9]+[.][0-9]+)(.*)".r
      private val num             = s"$opReg([+-]?[0-9]+)(.*)".r
      
      def mutate(in: Params, n: Int, rate: Int): Result = Option {
        val mutantsToBe = shuffle(in.filter(_ matches ".*[0-9].*").toBuffer).take(n)
        var out = in
        var mutationType = ""
        for(nonMutant <- mutantsToBe) {
          def change: Int = if(nextBoolean) -rate else rate
          val mutant = nonMutant match { //TODO: think about if max(0, is okay
            case percentXpercentColor(op, num1, num2, grey, num3, tail) => mutationType += "(percentXpercent color)"
              op + max(0, num1.toInt + change*nextInt(1)) + "%x" + max(0, num2.toInt+change*nextInt(1)) + grey + min(100, max(0, num3.toInt + change*nextInt(3))) + tail
            case colorColor(op, c1, num1, c2, num2) => mutationType += "(color,color)"
              op + c1 + min(100, max(0, num1.toInt + change*nextInt(3))) + c2 + min(100, max(0, num2.toInt + change*nextInt(3)))
            case percentXpercent(op, num1, num2, tail) => mutationType += "(percentXpercent)"
              op + max(0, num1.toInt + change*nextInt(3)) + "%x" + max(0, num2.toInt+change*nextInt(3)) + tail
            case percent(op, num, tail) => mutationType += "(percent)"
              op + max(0, num.toInt + change*(nextInt(2)+1)) + tail
            case numXpercent(op, num1, num2, tail) =>  mutationType += "(numXpercent)"
              op + max(0, num1.toInt + change*nextInt(3)) + "x" + max(0, num2.toInt + change*nextInt(3)) + tail
            case floatXfloat(op, num1, num2, tail) => mutationType += "(floatXfloat)"
              op + (num1.toDouble + change*nextDouble).toString.take(4) + "x" + (num2.toDouble + change*nextDouble).toString.take(4) + tail
            case numXfloat(op, num1, num2, tail) => mutationType += "(numXfloat)"
              op + max(0, num1.toInt + change*nextInt(3)) + "x" + (num2.toDouble + change*nextDouble).toString.take(4) + tail
            case numXnum(op, num1, num2, tail) => mutationType += "(numXnum)"
              op + max(0, num1.toInt + change*nextInt(3)) + "x" + max(0, num2.toInt + change*nextInt(3)) + tail
            case float(op, num, tail) => mutationType += "(float)"
              op + (num.toDouble + change*nextDouble/5).toString.take(4) + tail
            case num(op, num, tail) => mutationType += "(num)"
              op + max(0, num.toInt + change*(nextInt(2)+1)) + tail
            case _ =>
              //Seq("xmessage", in.mkString(" ")).!
              nonMutant
          }
          
          out = out.map(param => 
            if(param == nonMutant) {
              mutant
            } else {
              param
            }
          ) //TODO: bug: if two switches are same, this mutates both, also check if switches are same
        }
        
        (out, mutationType)
      }
    }
    object Alter extends Phase(name = "Alter", stepLimit = 25, failLimit = -1) {
      override def precondition(params: Params): Boolean = params.exists(canAlterParam)

      val alterLists = List(
        List("-black-threshold", "-white-threshold", "-threshold", "-solarize"),
        List("-liquid-rescale", "-scale", "-extent"),
        List("+distort SRT", "-distort SRT", "-rotate", "-shear"),
        List("Edge", "Dither", "Tile", "Random").map(param => "-virtual-pixel "+param),
        
        List("-morphology Erode", "-morphology ErodeI").map(param => "-morphology "+param),
        List("-morphology Dilate", "-morphology DilateI").map(param => "-morphology "+param),
        List("-morphology Open", "-morphology OpenI").map(param => "-morphology "+param),
        List("-morphology Close", "-morphology CloseI").map(param => "-morphology "+param),
        List("Dilate","Smooth","Erode","Open","Close","Edge","EdgeIn","EdgeOut", "Convolve").map(param => "-morphology "+param),
        
        List("NearestNeighbor","Average","Average4","Average9","Bilinear").map(param => "-interpolate "+param)
      )
      //val alterFuncs = Map[(String,String), (String => String)] { ("-rotate","-shear") -> (a: String => a + (a.reverse.takeWhile(_ != ' ')*2)) } withDefault(s: String => s)
      val alterSet = alterLists.reduce(_ ++ _) //TODO: more efficient datastructure... prefix trie?
      
      def canAlterParam(param: String): Boolean = alterSet.exists(prefix => param.startsWith(prefix))
      def alterParam(param: String): String = {
        var paramPrefix = ""
        val outList = ((alterLists find { list => 
          list exists { prefix => 
            paramPrefix = prefix //Sorry for the side-effect :P
            param.startsWith(prefix)
          }
        } get).toSet - paramPrefix).toList
        
        val out = param.replace(paramPrefix: CharSequence, outList.random: CharSequence)
        println((param, out))
        out
      }

      @tailrec def alter(l: Params, n: Int = 1, doneIndices: HashSet[Int] = HashSet()): Result = {
        val out = l.toArray
        val indices = (0 until out.size).filter(i => !doneIndices.contains(i) && canAlterParam(out(i))).toList
        if(n == 0 || indices.isEmpty) return Option((l, doneIndices.mkString(",")))
        var hasAltered = false
        while(!hasAltered) {
          val i = indices.random
          if(nextDouble < 0.25) {
            out(i) = alterParam(out(i))
            hasAltered = true
            doneIndices += i
          }
        }
        alter(out.toList, min(n-1, indices.size-1), doneIndices)
      }

      override def applyPhase(params: Params): Result = alter(params, step%2 + 1)
    }
    
    object Multiple extends Phase(name = "Multiple", stepLimit = 75, failLimit = -1) {
      val phases = List(Insert, Swap, Mutate, Alter,Trim)
      override def precondition(params: Params): Boolean = phases.exists(_.precondition(params))
      override def applyPhase(params: Params): Result = Option {
        val goodPhases = phases.filter(_.precondition(params))
        println("hi")
        for(phase <- goodPhases) println(phase.name)
        if(goodPhases.isEmpty) (None,"")
        val howMany = nextInt(2) + 2
        var howManyPassed = 0
        val toApply = for(i <- 0 until howMany) yield goodPhases.random
        val out = toApply.foldLeft((params, "")){ (current, phase) => 
          val result = phase.applyPhase(current._1)
          result map { res => howManyPassed += 1; (res._1, current._2+phase.name+res._2) } orElse (Some(current)) get
        }
        
        (out._1, s"($howManyPassed/$howMany)${out._2}")
      }
    }

    val phases = List(Insert, Trim, Swap, Mutate, Alter, Multiple)

    // Best, last best, cyclic best, wild card, short best
    var best, lBest, cBest, wBest, sBest = (emptyParams, (0d,0d))
    var finished = false
    def finish(quit: Boolean = true) { 
      println()
      println(best)
      val ocr = images.map(img => (OCR(img._1, engine, best._1), img._2))
      ocr.foreach(println)
      ((s"""echo engine ${engine}: (${best._2._1.toString.take(10)},${best._2._2.toString.take(10)}) - ${best._1.mkString(", ")}""") #>> new java.io.File("aaa")).!
      //TODO: make it output bash script for reading: https://gist.github.com/HairyFotr/4588073
      //(Seq("echo", s"""#engine ${engine}: ${best._2._1.toString.take(10)}, ${best._2._2.toString.take(10)}""") #>> new java.io.File("aaa")).!
      //(Seq("echo", s"""OCR() { convert $$1 ${best._1.mkString(", ")} ??? """) #>> new java.io.File("aaa")).!
      (Seq("echo", best._1.mkString(", ")) #>> new java.io.File("preload2")).!
      if(quit) sys.exit(0) else finished = true
    }
    
    var lasthit = 0
    for(i <- 0 to 3500) if(!finished) {
      var phaseType = ""
      val params: Params = if(i == -1) List() else {
        val candidates = List(best._1, lBest._1, cBest._1, wBest._1, sBest._1)
        
        def get: Params = {
          val (out, pType) = 
            if(i < 500) {
              Preload(candidates: _*)
                .orElse(Multiple(List()))
                .get
            } else {
              Preload(candidates: _*)
                .orElse(Swap(best._1))
                .orElse(Mutate(candidates: _*))
                .orElse(Alter(candidates: _*))
                .orElse(Trim(best._1))
                .orElse(Insert(candidates: _*))
                .orElse(Multiple(candidates: _*))
                .get
            }
          
          phaseType = pType
          out
        }

        // LOL @ this part - the ugly thing that survived the refactoring
        var out: Params = null
        println(phaseType)
        def gget: Params = try { get } catch { case e: Exception => phases.foreach(_.reset); phaseType += s" FAIL $e"; gget }
            
        try { out = gget } catch { case e: Throwable => for(result <- ResultCache.oldResults.toList.sorted) println(result); sys.exit(-1) }
        println(phaseType)

        newResult(out)
        out
      }
      
      val ocr = images.map(img => (OCR(img._1, engine, params), img._2))
      
      postprocess(engine)

      // % of correct reads
      lazy val imgScore = ocr.map(res => if(res._1 == res._2) 1d else 0d).sum / ocr.size.toDouble

      // % of correct words
      lazy val wordScore = (
        ocr
          .map(res => (res._1.split(" ").toSet, res._2.split(" ").toSet))
          .map(res => (res._1 & res._2).size / res._2.size.toDouble)
          .sum / ocr.size.toDouble)
      
      // compare strings with the Levenshtein distance (slow / crashing for long strings)
      lazy val levScore = ocr.map(res => 1 - (levDistance(res._1, res._2) / res._2.size.toDouble)).sum / ocr.size.toDouble
      
      // compare strings char by char from the left and from the right
      lazy val matchScore = (
        ocr
          .map(res => 
            (res._1.zip(res._2).count(char => char._1 == char._2) + 
             res._1.reverse.zip(res._2.reverse).count(char => char._1 == char._2)
            )/(2*res._2.size.toDouble))
          .sum / ocr.size.toDouble)
      
      val score = (wordScore, matchScore)
      
      val status = s"""|
                       |engine: $engine
                       |  iter: $i
                       | phase: $phaseType
                       |params: ${params.mkString(" ")}
                       | score: ${imgScore}
                       | score: ${wordScore}
                       | score: ${matchScore}
                       |""".stripMargin
                       
      println(status)
      
      if(score._1 > best._2._1 || (score._1 == best._2._1 && (score._2 > best._2._2 || (score._2 == best._2._2 && params.mkString.size < best._1.mkString.size)))) {
        lasthit = i
        cBest = lBest
        lBest = best
        best = (splitParams(params), score)
        (Seq("echo", status) #>> new java.io.File("statuses")).!
        (Seq("echo", status) #> new java.io.File("status")).!
        (Seq("echo", ocr.map(res => if(res._1 == res._2) "!!"+res else ""+res).mkString("\n")) #>> new java.io.File("status")).!
      } else if((score._1 > sBest._2._1 && params.mkString.size < min(lBest._1.mkString.size, best._1.mkString.size)) || (sBest._2._1 == 0 && score._2 > sBest._2._2)) {
        sBest = (splitParams(params), score)
      } else if((score._1 >= cBest._2._1 && (score._1 > wBest._2._1 || (score._1 == wBest._2._1 && score._2 > wBest._2._2)) && nextBoolean) || (wBest._2._2 == 0d && score._2 > wBest._2._2)) {
        wBest = (splitParams(params), score)
      } else if(lBest._2 == (0d,0d)) {
        lBest = (splitParams(params), score)
      } else if(cBest._2 == (0d,0d)) {
        cBest = (splitParams(params), score)
      }
      
      if(i - lasthit > 500) {
        finish(false)
      }
    }
    finish(false)
  }
}

object Utils {
  // Levenshtein distance
  def levDistance(s1: String, s2: String): Int = {
    val memo = collection.mutable.Map[(List[Char], List[Char]), Int]()
    def min3(a: Int, b: Int, c: Int): Int = min(min(a, b), c)
    def sd(s1: List[Char], s2: List[Char]): Int =
      memo.getOrElseUpdate((s1, s2), 
        (s1, s2) match {
          case (_, Nil) => s1.length
          case (Nil, _) => s2.length
          case (c1 :: t1, c2 :: t2) => 
            min3(sd(t1,s2) + 1, 
                 sd(s1,t2) + 1,
                 sd(t1,t2) + (if(c1 == c2) 0 else 1))
        })

    sd(s1.toList, s2.toList)
  }
    
  def fromFile(name: String): List[String] = {
    val file = io.Source.fromFile(name)
    val out = file.getLines.toList
    file.close
    out
  }

  implicit class Seqs[A](val s: Seq[A]) { 
    def random: A = s(nextInt(s.size)) 
    def randomOption: Option[A] = if(s.size > 0) Some(random) else None
  }

  def insert[A](l: List[A], i: Int, elt: A): List[A] = (l.take(i) :+ elt) ++ l.drop(i)
  def drop[A](l: List[A], i: Int, n: Int = 1): List[A] = l.take(i) ++ l.drop(i+n)
  def swap(l: List[String], i: Int, j: Int): List[String] = {
    val a = l.toArray
    val s = a(i)
    a(i) = a(j)
    a(j) = s
    a.toList
  }
}
