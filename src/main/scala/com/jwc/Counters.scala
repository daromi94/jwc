package com.jwc

enum CountOption(val value: Char):
  case Bytes extends CountOption('c')
  case Lines extends CountOption('l')
  case Words extends CountOption('w')

object CountOption:
  def from(value: Char): Option[CountOption] =
    CountOption.values.find(o => o.value == value)
