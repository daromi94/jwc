package com.daromi.jwc.core

enum CountOption:
  case Bytes, Lines, Words

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

private def countBytes(bytes: Array[Byte]): CountValue =
  bytes.length

private def countLines(bytes: Array[Byte]): CountValue =
  bytes.count(_ == 0x0a)

private def countWords(bytes: Array[Byte]): CountValue =
  val whitespaces = Set(0x09, 0x0a, 0x0d, 0x20)
  val isWhitespace = whitespaces.contains _

  var count = 0L
  var inside = false

  for byte <- bytes
  do
    if !inside && !isWhitespace(byte) then
      inside = true
      count = count + 1
    else if inside && isWhitespace(byte) then inside = false

  count
