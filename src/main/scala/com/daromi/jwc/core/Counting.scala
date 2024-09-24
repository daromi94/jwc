package com.daromi.jwc.core

enum CountOption:
  case Bytes, Lines, Words

type CountValue = Long
type CountByOption = Map[CountOption, CountValue]

def count(
    bytes: Array[Byte],
    options: Set[CountOption]
): CountByOption =
  def internal(
      option: CountOption,
      bytes: Array[Byte]
  ): (CountOption, CountValue) =
    val value = option match
      case CountOption.Bytes => countBytes(bytes)
      case CountOption.Lines => countLines(bytes)
      case CountOption.Words => countWords(bytes)

    (option, value)

  options
    .map(option => internal(option, bytes))
    .map((option, value) => option -> value)
    .toMap

private def countBytes(bytes: Array[Byte]): CountValue =
  bytes.length

private def countLines(bytes: Array[Byte]): CountValue =
  bytes.count(byte => byte == 10)

private def countWords(bytes: Array[Byte]): CountValue =
  def isNonWhitespace(byte: Byte): Boolean =
    (33 <= byte && byte <= 126) || (128 <= byte && byte <= 255)

  var count = 0L
  var inside = false

  for byte <- bytes
  do
    if !inside && isNonWhitespace(byte) then
      inside = true
      count = count + 1
    else if inside && !isNonWhitespace(byte) then inside = false

  count
