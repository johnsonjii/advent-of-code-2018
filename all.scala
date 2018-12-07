object Util {
  type Input = Seq[String]

  def inputFromFile(name: String): Input =
    io.Source.fromFile(name).getLines.toSeq

  def inputFromString(str: String): Input = str.lines.toSeq
}

object Solutions {
  import Util._

  object day01 { 

    def puzzel1(in: Input): Int =
      in.map(_.toInt).foldLeft(0)(_ + _)

    def puzzel2(in: Input): Int = {
      val stream = Stream.continually(in.map(_.toInt)).flatMap(identity)
      var running = 0
      val all = collection.mutable.Set(0)
      var dup: Option[Int] = None

      stream.find { x =>
        running = running + x
        if (!all.add(running))
          dup = Some(running)
        dup.nonEmpty
      }

      dup.get
    }
  }

  object day02 {

    def puzzel1(in: Input): Int = {
      in.flatMap(_.groupBy(identity).values.find(_.size == 2)).size *
      in.flatMap(_.groupBy(identity).values.find(_.size == 3)).size
    }

    def puzzel2(in: Input): Unit = {
      (0 until in.head.length).flatMap(i => in
        .map(s => (s.take(i) ++ s.drop(i+1)).mkString)
        .groupBy(identity)
        .values
        .find(_.size > 1)
        .map(_.head))
        .foreach(println)
    }
  }

  object day03 {
    import collection.{mutable => m}

    type FabricMatrix = m.Map[Int, m.Map[Int, m.ArrayBuffer[Int]]]

    case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int) {
      def register(matrix: FabricMatrix): Unit = {
        for {
          x <- left until left + width
          y <- top until top + height
        } matrix
          .getOrElseUpdate(y, m.Map().empty)
          .getOrElseUpdate(x, m.ArrayBuffer.empty)
          .append(id) 
      }

      def unique(matrix: FabricMatrix): Option[Claim] = {
        var isGood = true
        for {
          x <- left until left + width
          y <- top until top + height
          } {
            val ids = matrix(y)(x)
            if (ids.size > 1 || ids.head != id) isGood = false
          }
          if (isGood) Some(this) else None
      }
    }

    object Claim {
      private val c = "^\\D*(\\d*)\\D*(\\d*)\\D*(\\d*)\\D*(\\d*)\\D*(\\d*)\\D*$".r
      def apply(s: String): Claim = s match {
        case c(id, l, t, w, h) => Claim(id.toInt, l.toInt, t.toInt, w.toInt, h.toInt)
      }
    }

    def puzzel1(in: Input): Int = {
      val matrix: FabricMatrix = m.Map.empty
      in.map(Claim.apply).foreach(_.register(matrix))

      matrix.values.map(_.values.map({
        case x if x.size > 1 => 1
        case _ => 0
      }).sum).sum
    }

    def puzzel2(in: Input): Int = {
      val matrix: FabricMatrix = m.Map.empty
      val claims = in.map(Claim.apply)
      claims.foreach(_.register(matrix))
      claims.flatMap(_.unique(matrix)).head.id
    }
  }

  object day04 {
    import java.time.LocalDateTime

    case class Log(time: LocalDateTime, detail: String)

    private implicit class LogString(s: String) {
      def toLog: Log = Log(LocalDateTime.parse(s.drop(1).take(10) + "T" + s.drop(12).take(5)), s.drop(19))
    }

    val guardNo = "^\\D*(\\d*) .*$".r

    def parseGuards(in: Input): Map[Int, List[(Int, Int)]] = {

      val zo = java.time.ZoneOffset.of("+0000")
      val logs = in.map(_.toLog).sortBy(_.time.toEpochSecond(zo))
      logs
        .foldLeft(Seq[Seq[Log]]())({(g, l) =>
          if (l.detail startsWith "Guard") g :+ Seq(l)
          else g.init :+ (g.last :+ l)
        })
        .groupBy({
          case h :: _ => guardNo.findFirstMatchIn(h.detail).get.group(1).toInt
        })
        .mapValues(_.flatMap({
          case _ :: t => t.map(_.time.getMinute).grouped(2).map({
            case Seq(a, b) => (a, b - a)
          })
        }).toList)
    }

    def puzzel1(in: Input): Int = {
      val (guard, minutes) = parseGuards(in).maxBy(_._2.map(_._2).sum)
      val minute = minutes.flatMap({
        case (s, l) => (s until s + l).toList
      }).groupBy(identity).maxBy(_._2.size)._1

      guard * minute
    }

    def puzzel2(in: Input): Int = {
      val guards = parseGuards(in)
      val (guard, (minute, _)) = guards
        .filterNot(_._2.isEmpty)
        .mapValues(_.flatMap({
          case (s, l) => (s until s + l).toList
        })
          .groupBy(identity)
          .map(m => (m._1, m._2.size))
          .maxBy(_._2))
        .maxBy(_._2._2)

      guard * minute
    }
  }

  object day05 {

    case class Unit(t: String, positive: Boolean)
    object Unit {
      def apply(c: Char): Unit = {
        val s = c.toString
        val t = s.toLowerCase
        Unit(t, s == t)
      }
    }

    def react(poly: Seq[Unit]): Int = {
      var units: Seq[Unit] = poly
      var continue = true
      while (continue) {
        units.sliding(2).zipWithIndex.find({case (Seq(a, b), _) => a.t == b.t && a.positive != b.positive}) match {
          case Some((_, index)) => units = units.take(index) ++ units.drop(index + 2)
          case None => continue = false
        }
      }
      units.length
    }

    def puzzel1(in: Input): Int = react(in.head.toSeq.map(Unit.apply))

    def puzzel2(in: Input) = {
      val fullPoly: Seq[Unit] = in.head.toSeq.map(Unit.apply)
      (for (u <- fullPoly.map(_.t).distinct) yield {
        (u, react(fullPoly.filterNot(_.t == u)))
      }).map(_._2).min
    }
  }

  object day06 {
    val testInput = Util.inputFromString(
      """1, 1
        |1, 6
        |8, 3
        |3, 4
        |5, 5
        |8, 9""".stripMargin)

    private case class Coord(x: Int, y: Int)

    private trait Distance {
      def id: Int
      def len: Int
    }

    private case class UniqueDistance(id: Int, len: Int) extends Distance {
      override def toString = (id + (if (len == 0) 65 else 97)).toChar.toString
    }

    private case object UndefinedDistance extends Distance {
      val id = -1
      val len = Int.MaxValue
      override def toString = "_"
    }

    private case class SharedDistance(len: Int) extends Distance {
      val id = -2
      override def toString = "."
    }

    import reflect.ClassTag

    private def parseInput[T: ClassTag](in: Input, fill: T): (List[Coord], Int, Int, Int, Int, Array[Array[T]]) = {
      val cc = in.map { l =>
        val Array(x, y) = l.split(", ")
        Coord(x.toInt, y.toInt)
      }.toList
      val l = cc.map(_.x).min
      val t = cc.map(_.y).min
      val r = cc.map(_.x).max + 1
      val b = cc.map(_.y).max + 1
      val matrix = Array.fill[T](b - t, r - l)(fill)
      (cc.map(c => Coord(c.x - l, c.y - t)),
        l, t, r, b, matrix)
    }

    def puzzel1(in: Input): Int = {
      var (cc, l, t, r, b, matrix) = parseInput[Distance](in, UndefinedDistance)

      for {
        (c, id) <- cc.zipWithIndex
        x <- 0 until r - l
        y <- 0 until b - t
      } {
        val len = math.abs(x - c.x) + math.abs(y - c.y)
        val current = matrix(y)(x)
        matrix(y)(x) =
          if (current.len > len) UniqueDistance(id, len)
          else if (current.len == len) SharedDistance(len)
          else current
      }

      // println(matrix.map(_.mkString).mkString("\n"))

      val infiniteIds = Seq(matrix.head,
        matrix.last,
        matrix.init.tail.flatMap(row => Seq(row.head, row.last)))
          .flatten
          .map(_.id)
          .toSet

      matrix.flatten.collect({
        case UniqueDistance(id, _) if !infiniteIds.contains(id) => id
      }).groupBy(identity).values.map(_.size).max
    }

    def puzzel2(in: Input): Int = {
      var (cc, l, t, r, b, matrix) = parseInput[Boolean](in, false)

      for {
        x <- 0 until r - l
        y <- 0 until b - t
      } {
        if (cc.map(c => math.abs(x - c.x) + math.abs(y - c.y)).sum < 10000)
          matrix(y)(x) = true
      }

      matrix.flatten.filter(identity).size
    }
  }
}
