import java.time.{ZonedDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter

case class Results(benchmark: String, pr: Int, merged: Boolean, commitTime: ZonedDateTime, commit: String, benchTime: ZonedDateTime, warmup: Seq[Double], measures: Seq[Double]):
  assert(commitTime.getZone() == ZoneOffset.UTC, s"expected commit time '$commitTime' to be in UTC")
  assert(benchTime.getZone() == ZoneOffset.UTC, s"expected benchmark time '$benchTime' to be in UTC")
  assert(benchTime.isAfter(commitTime), s"expected benchmark time '$benchTime' to be after commit time '$commitTime'")

  def toCSVRow(): Seq[String] = Seq(
    benchmark,
    pr.toString,
    merged.toString,
    commitTime.format(DateTimeFormatter.ISO_DATE_TIME),
    commit,
    benchTime.format(DateTimeFormatter.ISO_DATE_TIME),
    warmup.mkString(" "),
    measures.mkString(" ")
  )

object Results:
  def fromCSVRow(row: Seq[String]): Results =
    val Seq(benchmark, pr, merged, commitTime, commit, benchTime, warmupRaw, measuresRaw) = row
    Results(
      benchmark,
      pr.toInt,
      merged.toBoolean,
      ZonedDateTime.parse(commitTime),
      commit,
      ZonedDateTime.parse(benchTime),
      warmupRaw.trim().split(" ").toSeq.map(_.toDouble),
      measuresRaw.trim().split(" ").toSeq.map(_.toDouble)
    )
