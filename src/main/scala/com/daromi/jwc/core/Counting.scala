package com.daromi.jwc.core

enum CountOption(val value: Char):
  case Bytes extends CountOption('c')
  case Lines extends CountOption('l')
  case Words extends CountOption('w')

object CountOption:
  val DEFAULTS: Set[CountOption] = Set(Bytes, Lines, Words)

  def from(value: Char): Option[CountOption] = values.find(_.value == value)

  def exists(value: Char): Boolean = values.exists(_.value == value)

type CountValue = Long
type CountByOption = Map[CountOption, CountValue]

def count(bytes: Array[Byte], options: Set[CountOption]): CountByOption =
  def internal(
      option: CountOption,
      bytes: Array[Byte]
  ): (CountOption, CountValue) =
    val value = option match
      case CountOption.Bytes => countBytes(bytes)
      case CountOption.Lines => countLines(bytes)
      case CountOption.Words => countWords(bytes)

    (option, value)

  options.map(internal(_, bytes)).toMap

private def countBytes(bytes: Array[Byte]): CountValue = bytes.length

private def countLines(bytes: Array[Byte]): CountValue = bytes.count(_ == 0x0a)

private def countWords(bytes: Array[Byte]): CountValue =
  val whitespaces = Set[Byte](0x09, 0x0a, 0x0d, 0x20)
  val isWhitespace = whitespaces.contains

  var count = 0L
  var inside = false

  for byte <- bytes
  do
    if !inside && !isWhitespace(byte) then
      inside = true
      count = count + 1
    else if inside && isWhitespace(byte) then
      inside = false

  count
