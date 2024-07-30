package com.jwc

type CountValue = Long

enum CountOption(val value: Char):
  case Bytes extends CountOption('c')
  case Lines extends CountOption('l')
  case Words extends CountOption('w')

object CountOption:
  def from(value: Char): Option[CountOption] =
    CountOption.values.find(o => o.value == value)

type Count = Map[CountOption, CountValue]

def count(
    bytes: Array[Byte],
    countOptions: Set[CountOption]
): Count =
  def delegate(
      countOption: CountOption,
      bytes: Array[Byte]
  ): (CountOption, CountValue) =
    countOption match
      case CountOption.Bytes => (countOption, countBytes(bytes))
      case CountOption.Lines => (countOption, countLines(bytes))
      case CountOption.Words => (countOption, countWords(bytes))

  countOptions.map(o => delegate(o, bytes)).map((o, v) => o -> v).toMap

def countBytes(bytes: Array[Byte]): CountValue =
  bytes.length

def countLines(bytes: Array[Byte]): CountValue =
  // REVIEW
  bytes.count(b => b == 10)

def countWords(bytes: Array[Byte]): CountValue =
  // FIXME: ignores consecutive whitespaces
  bytes.count(b => b == 32) + 1
