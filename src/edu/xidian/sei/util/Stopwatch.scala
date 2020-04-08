package edu.xidian.sei.util

/**
  * Stopwatch
  * Time statistics tool class
  */
object Stopwatch {
  import java.util.concurrent.TimeUnit
  import java.util.concurrent.TimeUnit._
  /**
   * Returns a string representation of the current elapsed time, choosing an
   * appropriate unit and using the specified number of significant figures.
   * For example, at the instant when {@code elapsedTime(NANOSECONDS)} would
   * return {1234567}, {@code toString(4)} returns {@code "1.235 ms"}.
   */
  def format(nanos: Long, significantDigits: Int=4): String = {
    val unit = chooseUnit(nanos)
    val value = nanos.toDouble / NANOSECONDS.convert(1, unit)
    String.format("%." + significantDigits + "g %s", value.asInstanceOf[AnyRef], abbreviate(unit).asInstanceOf[AnyRef])
  }

  private def chooseUnit(nanos: Long): TimeUnit = {
    if (SECONDS.convert(nanos, NANOSECONDS) > 0) return SECONDS
    if (MILLISECONDS.convert(nanos, NANOSECONDS) > 0) return MILLISECONDS
    if (MICROSECONDS.convert(nanos, NANOSECONDS) > 0) return MICROSECONDS
    NANOSECONDS
  }

  private def abbreviate(unit: TimeUnit): String = unit match {
    case NANOSECONDS => "ns"
    case MICROSECONDS => "μs"
    case MILLISECONDS => "ms"
    case SECONDS => "s"
    case _ => throw new AssertionError()
  }
}