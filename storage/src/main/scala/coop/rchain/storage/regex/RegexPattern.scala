package coop.rchain.storage.regex

import scala.annotation.tailrec
import scala.util.Try

trait ParsedPattern {

  /**
    * Tries to convert a given Regex from string to a RegexPattern object
    * Returns parsed (Alt|Mult|Conc|CharClass)Pattern and position where
    * parsing finished
    */
  def tryParse(str: CharSequence): Option[(RegexPattern, Int)]

  /**
    * Converts entire given string to a RegexPattern object
    * Returns None if part of the string is a valid regex
    */
  def parse(str: CharSequence): Option[RegexPattern] =
    tryParse(str).flatMap {
      case (pattern, len) =>
        if (len == str.length)
          Some(pattern)
        else
          None
    }
}

/**
  * A companion object for the RegexPattern class
  */
object RegexPattern extends ParsedPattern {

  /**
    * Turn the supplied finite state machine into a `RegexPattern` object.
    * This is accomplished using the Brzozowski algebraic method.
    */
  def fromFsm(fsm: Fsm): RegexPattern =
    throw new NotImplementedError("TODO")

  /**
    * Tries to convert a given Regex from string to a RegexPattern object
    * Returns parsed (Alt|Mult|Conc|CharClass)Pattern and position where
    * parsing finished
    */
  def tryParse(str: CharSequence): Option[(RegexPattern, Int)] = AltPattern.tryParse(str)

  /**
    * This RegexPattern expresses "no possibilities at all"
    * and can never match anything.
    */
  val nothing: RegexPattern = CharClassPattern(Nil)
}

/**
  * Parent abstract class for all Regex patterns.
  * All patterns have some things in common. This parent class mainly
  * hosts documentation though.
  */
sealed abstract class RegexPattern {

  /**
    * Two RegexPatterns are equivalent if they recognise the same strings. Note
    *	that in the general case this is actually quite an intensive calculation,
    *	but far from unsolvable, as we demonstrate here:
    */
  def equivalent(that: RegexPattern): Boolean

  /**
    * Concatenate a sequence of Regex patterns, regardless of differing classes.
    * Call using "a = b + c"
    */
  def concatenate(that: RegexPattern): ConcPattern

  /**
    * Alternate between any two Regex patterns, regardless of differing classes.
    */
  def union(that: RegexPattern): RegexPattern

  /**
    * Takes the current RegexPattern and simplifies it in every way possible,
    * returning a simpler RegexPattern which is quite probably not of the same class
    * as the original. Approaches are different for every descendant.
    */
  def reduced: RegexPattern

  /**
    * Return a set of all unique characters used in this RegexPattern.
    * By convention, fsm.anythingElse is always included in this result.
    */
  lazy val alphabet: Set[Char] = Set(Fsm.anythingElse)

  /**
    * Intersection function. Return a RegexPattern that can match any string
    *	that both self and other can match. Fairly elementary results relating
    *	to regular languages and finite state machines show that this is
    * possible, but implementation is a BEAST in many cases. Here, we convert
    *	both Regex patters to FSMs (see to_fsm(), above) for the intersection, then
    *	back to RegexPatterns afterwards.
    *	Call using "a = b & c"
    */
  def intersection(that: RegexPattern): RegexPattern

  /**
    * Return a RegexPattern which will match any string which, when reversed,
    * self would match. E.g. if self matches "beer" then reversed(self) will
    * match "reeb".
    */
  def reversed: RegexPattern

  /**
    * Equivalent to repeated concatenation. Multiplier consists of a minimum
    * and a maximum; maximum may be infinite (for Kleene star closure).
    * Call using "a = b * qm"
    */
  def multiply(multiplier: Multiplier): MultPattern

  /**
    * Equivalent to repeated concatenation. Multiplier consists of a minimum
    * and a maximum; maximum may be infinite (for Kleene star closure).
    * Call using "a = b * qm"
    */
  final def multiply(multiplier: Int): MultPattern = multiply(Multiplier(multiplier))

  /**
    * Return False if there exists a string which the present RegexPattern
    * can match. Return true if no such string exists. Example of empty
    * patterns: CharClassPattern()
    */
  def isEmpty: Boolean

  /**
    * Test whether the present RegexPattern accepts the supplied string.
    * Used only for unit-testing.
    */
  private[regex] def accepts(s: String): Boolean = toFsm().accepts(s)

  def toFsm(alphabet: Option[Set[Char]] = None): Fsm

  final def toFsm(alphabet: Set[Char]): Fsm = toFsm(Some(alphabet))

  final override def equals(obj: scala.Any): Boolean = obj match {
    case pattern: RegexPattern => equivalent(pattern)
    case _                     => false
  }

  //region Operators

  /**
    * Just calls equivalent method
    */
  final def ==(that: RegexPattern): Boolean = equivalent(that)

  /**
    * Just calls !equivalent method
    */
  final def !=(that: RegexPattern): Boolean = !equivalent(that)

  /**
    * Concatenate a sequence of Regex patterns, regardless of differing classes.
    */
  final def +(that: RegexPattern): ConcPattern = concatenate(that)

  /**
    * Alternate between any two Regex patterns, regardless of differing classes.
    */
  final def |(that: RegexPattern): RegexPattern = union(that)

  /**
    * Intersection between any two Regex patterns.
    */
  final def &(that: RegexPattern): RegexPattern = intersection(that)

  /**
    * Returns repeated concatenation of this RegexPattern
    */
  final def *(multiplier: Int): MultPattern = multiply(multiplier)

  final def *(multiplier: Multiplier): MultPattern = multiply(multiplier)

  //endregion
}

/**
  * Companion object for the CharClassPattern, used only for easy testing
  */
object CharClassPattern extends ParsedPattern {
  def apply(charSet: String): CharClassPattern = new CharClassPattern(charSet.toSet)
  def apply(charSet: Seq[Char]): CharClassPattern = new CharClassPattern(charSet.toSet)
  def apply(charSet: Set[Char]): CharClassPattern = new CharClassPattern(charSet)
  def apply(charSet: String, negateCharSet: Boolean): CharClassPattern =
    new CharClassPattern(charSet.toSet, negateCharSet)
  def apply(charSet: Seq[Char], negateCharSet: Boolean): CharClassPattern =
    new CharClassPattern(charSet.toSet, negateCharSet)
  def apply(charSet: Set[Char], negateCharSet: Boolean): CharClassPattern =
    new CharClassPattern(charSet, negateCharSet)

  private[this] val escapes =
    Map('t' -> '\t', 'r' -> '\r', 'n' -> '\n', 'f' -> '\f', 'v' -> '\u0011')

  private[this] val allSpecialChars = "\\\\[]|().?*+{}".toSet

  //region predefined char classes
  private[this] val wordCharClass: CharClassPattern = CharClassPattern(
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")

  private[this] val nonWordCharClass: CharClassPattern = CharClassPattern(
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz",
    negateCharSet = true)

  private[this] val digitsCharClass: CharClassPattern = CharClassPattern("0123456789")

  private[this] val nonDigitsCharClass: CharClassPattern =
    CharClassPattern("0123456789", negateCharSet = true)

  private[this] val spacesCharClass: CharClassPattern = CharClassPattern("\t\n\11\f\r ")

  private[this] val nonSpacesCharClass: CharClassPattern =
    CharClassPattern("\t\n\11\f\r ", negateCharSet = true)
  //endregion

  def tryParse(str: CharSequence): Option[(CharClassPattern, Int)] = {

    def parseHexChar(startIndex: Int, charsCount: Int): Option[(Char, Int)] =
      if (startIndex + charsCount <= str.length) {
        val substr = str.subSequence(startIndex, startIndex + charsCount).toString
        Try(Integer.parseInt(substr, 16)).toOption
          .flatMap(codePoint => Some((codePoint.asInstanceOf[Char], startIndex + charsCount)))
      } else {
        None
      }

    def parseEscapedSequence(startIndex: Int): Option[(CharClassPattern, Int)] =
      if (startIndex < str.length) {
        str.charAt(startIndex) match {
          case 'w' =>
            Some((wordCharClass, startIndex + 1))
          case 'W' =>
            Some((nonWordCharClass, startIndex + 1))
          case 'd' => Some((digitsCharClass, startIndex + 1))
          case 'D' => Some((nonDigitsCharClass, startIndex + 1))
          case 's' => Some((spacesCharClass, startIndex + 1))
          case 'S' => Some((nonSpacesCharClass, startIndex + 1))
          case 'x' => {
            //unicode escape hex ascii sequence
            val hexChar = parseHexChar(startIndex + 1, 2)
            hexChar.flatMap {
              case (char, finalIndex) => Some(CharClassPattern(Set(char)), finalIndex)
            }
          }
          case 'u' => {
            //escape hex unicode sequence
            val hexChar = parseHexChar(startIndex + 1, 4)
            hexChar.flatMap {
              case (char, finalIndex) => Some(CharClassPattern(Set(char)), finalIndex)
            }
          }
          case esc if escapes.contains(esc) =>
            Some((CharClassPattern(Set(escapes(esc))), startIndex + 1))
          case other => Some((CharClassPattern(Set(other)), startIndex + 1))
        }
      } else {
        None
      }

    def addChar(currentChars: List[Char], addChar: Char, inRange: Boolean): List[Char] =
      if (inRange) {
        (currentChars.head to addChar).toList ++ currentChars
      } else {
        addChar :: currentChars
      }

    def parseInternalEscapedSequence(startIndex: Int): Option[(Char, Int)] =
      if (startIndex < str.length) {
        str.charAt(startIndex) match {
          case 'x'                          => parseHexChar(startIndex + 1, 2) //escape hex ascii sequence
          case 'u'                          => parseHexChar(startIndex + 1, 4) //escaped hex unicode sequence
          case esc if escapes.contains(esc) => Some(escapes(esc), startIndex + 1)
          case other                        => Some(other, startIndex + 1) //any unsupported escape sequence - just char
        }
      } else {
        None
      }

    //this function parses interior of a char set [abc-xyz], assuming that
    //entry '[^' is handled outside, and finished parsing if ']' found
    //in case of any error, including absent of ']' returns None
    def parseCharSetSequence(startIndex: Int,
                             negateCharSet: Boolean): Option[(CharClassPattern, Int)] = {
      @tailrec
      def processNextChar(currentChars: List[Char],
                          currentIndex: Int,
                          inRange: Boolean): Option[(List[Char], Int)] =
        if (currentIndex < str.length) {
          str.charAt(currentIndex) match {
            case '\\' => {
              //'\?' escape sequence
              val charToAdd = parseInternalEscapedSequence(currentIndex + 1)
              if (charToAdd.isDefined) {
                //@tailrec restricts flatMap usage here
                val (nextChar, nextPos) = charToAdd.get
                processNextChar(addChar(currentChars, nextChar, inRange),
                                nextPos,
                                inRange = false)
              } else {
                //we got an error during parseInternalEscapedSequence,
                //for example invalid hex char, or end of string
                None
              }
            }
            case ']' =>
              if (inRange) {
                // closing square bracket when we're in range, something like '[a-]'
                // that's an error, finish with no result
                None
              } else {
                //we're done
                Some(currentChars, currentIndex + 1)
              }
            case '-' =>
              if (inRange) {
                //error found something like "[a--b]"
                None
              } else {
                //remember that we're in range, and continue from the next char
                processNextChar(currentChars, currentIndex + 1, inRange = true)
              }
            case x =>
              processNextChar(addChar(currentChars, x, inRange),
                              currentIndex + 1,
                              inRange = false)
          }
        } else {
          None
        }

      processNextChar(Nil, startIndex, inRange = false)
        .flatMap {
          case (chars, endIndex) => Some(CharClassPattern(chars, negateCharSet), endIndex)
        }
    }

    if (str.length > 0) {
      str.charAt(0) match {
        case '.'  => Some((CharClassPattern("", negateCharSet = true), 1))
        case '\\' => parseEscapedSequence(1)
        case '[' => {
          if (str.length > 1) {
            if (str.charAt(1) == '^')
              parseCharSetSequence(2, negateCharSet = true)
            else
              parseCharSetSequence(1, negateCharSet = false)
          } else {
            None
          }
        }
        case c if allSpecialChars.contains(c) => None
        case c                                => Some((CharClassPattern(Set(c)), 1))
      }
    } else {
      None
    }
  }
}

/**
  * A CharClass is basically a Set of symbols. The reason for the
  * CharClass object instead of using Set directly is to allow us to
  * set a "negated" flag. A CharClass with the negation flag set is assumed
  * to contain every symbol that is in the alphabet of all symbols but not
  * explicitly listed inside the Set. e.g. [{@literal ^}a]. This is very handy
  * if the full alphabet is extremely large, but also requires dedicated
  * combination functions.
  */
final case class CharClassPattern(charSet: Set[Char], negateCharSet: Boolean = false)
    extends RegexPattern {
  //chars should consist only of chars
  require(!charSet.contains(Fsm.anythingElse), "Charset can't contain Fsm.AnythingElse")

  override lazy val alphabet: Set[Char] = charSet + Fsm.anythingElse

  override def hashCode(): Int = if (negateCharSet) charSet.hashCode() else -charSet.hashCode()

  override def equivalent(that: RegexPattern): Boolean = that match {
    case thatCharClass: CharClassPattern =>
      (negateCharSet == thatCharClass.negateCharSet) && (charSet == thatCharClass.charSet)
    case _ => false
  }

  override def toFsm(alphabet: Option[Set[Char]] = None): Fsm = {
    val actualAlphabet = alphabet.getOrElse(this.alphabet)
    //0 is initial, 1 is final
    val map = if (negateCharSet) {
      //if negated, make a singular FSM accepting any other characters
      Map(0 -> (actualAlphabet -- charSet).map(_ -> 1).toMap)
    } else {
      //if normal, make a singular FSM accepting only these characters
      Map(0 -> charSet.map(_ -> 1).toMap)
    }
    Fsm(actualAlphabet, Set(0, 1), 0, Set(1), map)
  }

  override def reduced: CharClassPattern = this //Char classes cannot be reduced

  override def isEmpty: Boolean = charSet.isEmpty && !negateCharSet

  /**
    * Negate the current CharClass e.g. [ab] becomes [{@literal ^}ab].
    */
  def negated: CharClassPattern = CharClassPattern(charSet, !negateCharSet)

  /**
    * Concatenate a sequence of Regex patterns, regardless of differing classes.
    * Call using "a = b + c"
    */
  override def concatenate(that: RegexPattern): ConcPattern = multiply(1) + that

  override def multiply(multiplier: Multiplier): MultPattern = MultPattern(this, multiplier)

  /**
    * Alternate between any two Regex patterns, regardless of differing classes.
    * This method MUST NOT call the toFsm method, because this method is used
    * in turn when converting an FSM back to a regex.
    *
    * ¬A OR ¬B = ¬(A AND B)
    * ¬A OR B = ¬(A - B)
    * A OR ¬B = ¬(B - A)
    * A OR B
    */
  override def union(that: RegexPattern): RegexPattern = that match {
    case thatCharClass: CharClassPattern =>
      (this.negateCharSet, thatCharClass.negateCharSet) match {
        case (true, true) =>
          CharClassPattern(this.charSet & thatCharClass.charSet, negateCharSet = true)
        case (true, false) =>
          CharClassPattern(this.charSet -- thatCharClass.charSet, negateCharSet = true)
        case (false, true) =>
          CharClassPattern(thatCharClass.charSet -- this.charSet, negateCharSet = true)
        case (false, false) => CharClassPattern(this.charSet | thatCharClass.charSet)
      }
    case _ => multiply(1) | that
  }

  /**
    * Return a RegexPattern that can match any string
    *	that both self and other can match.
    *
    * ¬A AND ¬B = ¬(A OR B)
    * ¬A AND B = B - A
    * A AND ¬B = A - B
    * A AND B
    */
  override def intersection(that: RegexPattern): RegexPattern = that match {
    case thatCharClass: CharClassPattern =>
      (this.negateCharSet, thatCharClass.negateCharSet) match {
        case (true, true) =>
          CharClassPattern(this.charSet | thatCharClass.charSet, negateCharSet = true)
        case (true, false)  => CharClassPattern(thatCharClass.charSet -- this.charSet)
        case (false, true)  => CharClassPattern(this.charSet -- thatCharClass.charSet)
        case (false, false) => CharClassPattern(this.charSet & thatCharClass.charSet)
      }
    case _ => multiply(1) & that
  }

  /**
    * CharClass doesn't change when reversed
    */
  override def reversed: CharClassPattern = this

  /**
    * Returns RegexPattern.negated
    */
  def unary_~ : CharClassPattern = negated
}

object ConcPattern extends ParsedPattern {
  def apply(pattern: MultPattern): ConcPattern = new ConcPattern(pattern :: Nil)

  def apply(patterns: RegexPattern*): ConcPattern =
    ConcPattern(patterns.map {
      case mp: MultPattern => mp
      case p               => MultPattern(p, Multiplier.presetOne)
    }.toList)

  val presetEmptyString = ConcPattern(Nil)

  def tryParse(str: CharSequence): Option[(ConcPattern, Int)] = {
    @tailrec
    def parseRecursive(startPosition: Int, parsed: List[MultPattern]): (List[MultPattern], Int) =
      MultPattern.tryParse(str.subSequence(startPosition, str.length)) match {
        case Some((mult, pos)) => {
          val nextMults = mult :: parsed
          val nextPos = startPosition + pos
          if (nextPos < str.length) {
            //we have one more alternation option
            parseRecursive(nextPos, nextMults)
          } else {
            //no more alternation options
            (nextMults, nextPos)
          }
        }
        case _ => (parsed, startPosition)
      }

    val (seq, seqEndIndex) = parseRecursive(0, Nil)

    if (seq.nonEmpty) {
      val concPattern = ConcPattern(seq.reverse)
      Some(concPattern, seqEndIndex)
    } else {
      //nothing parsed, no characters eaten from input stream
      None
    }
  }
}

/**
  * A ConcatenationPattern is a tuple of mults i.e. an unbroken
  * string of mults occurring one after the other.
  * e.g. abcde[{@literal ^}fg]*h{4}[a-z]+(subpattern)(subpattern2)
  * To express the empty string, use an empty ConcatenationPattern().
  */
final case class ConcPattern(mults: List[MultPattern]) extends RegexPattern {

  override lazy val alphabet: Set[Char] = mults.flatMap(_.alphabet).toSet + Fsm.anythingElse

  override def hashCode: Int = mults.hashCode

  override def equivalent(that: RegexPattern): Boolean = that match {
    case concPattern: ConcPattern =>
      (mults.size == concPattern.mults.size) && mults.corresponds(concPattern.mults)(
        _.equivalent(_))
    case _ => false
  }

  override def reversed: ConcPattern = ConcPattern(mults.map(_.reversed).reverse)

  override def isEmpty: Boolean = mults.forall(_.isEmpty)

  override def toFsm(alphabet: Option[Set[Char]]): Fsm = {
    val actualAlphabet = alphabet.getOrElse(this.alphabet)
    //start with a component accepting only the empty string
    mults.foldLeft(Fsm.epsilonFsm(actualAlphabet))(_ + _.toFsm(actualAlphabet))
  }

  override def concatenate(that: RegexPattern): ConcPattern = {
    val thatConc: ConcPattern = that match {
      case thatConc: ConcPattern       => thatConc
      case thatChars: CharClassPattern => ConcPattern(thatChars * 1)
      case thatMult: MultPattern       => ConcPattern(thatMult)
      case thatAlt: AltPattern         => ConcPattern(thatAlt * 1)
    }

    ConcPattern(mults ++ thatConc.mults)
  }

  override def union(that: RegexPattern): RegexPattern = AltPattern(this) | that

  override def intersection(that: RegexPattern): RegexPattern = AltPattern(this) & that

  override def multiply(multiplier: Multiplier): MultPattern =
    MultPattern(AltPattern(this), multiplier)

  override def reduced: RegexPattern = throw new NotImplementedError("TODO")

  /**
    * Return the common prefix of these two ConcPatterns;
    * that is, the largest ConcPattern which can be safely beheaded()
    * from the front of both. The result could be emptystring.
    * "ZYAA, ZYBB" -> "ZY"
    * "CZ, CZ" -> "CZ"
    * "YC, ZC" -> ""
    * With the "suffix" flag set, works from the end. E.g.:
    * "AAZY, BBZY" -> "ZY"
    * "CZ, CZ" -> "CZ"
    * "CY, CZ" -> ""
    */
  def common(that: ConcPattern, suffix: Boolean = false): ConcPattern =
    throw new NotImplementedError("TODO")
}

object AltPattern extends ParsedPattern {
  def apply(pattern: ConcPattern): AltPattern = new AltPattern(Set(pattern))
  def apply(patterns: List[ConcPattern]): AltPattern = new AltPattern(patterns.toSet)
  def apply(patterns: CharClassPattern*): AltPattern =
    new AltPattern(patterns.map(cp => ConcPattern(cp)).toSet)

  def tryParse(str: CharSequence): Option[(AltPattern, Int)] = {
    @tailrec
    def parseRecursive(startPosition: Int, parsed: List[ConcPattern]): (List[ConcPattern], Int) =
      ConcPattern.tryParse(str.subSequence(startPosition, str.length)) match {
        case Some((conc, pos)) => {
          val nextConcs = conc :: parsed
          val nextPos = startPosition + pos
          if ((nextPos < str.length) && (str.charAt(nextPos) == '|')) {
            //we have one more alternation option
            parseRecursive(nextPos + 1, nextConcs)
          } else {
            //no more alternation options
            (nextConcs, nextPos)
          }
        }
        case _ => (parsed, startPosition)
      }

    val (seq, seqEndIndex) = parseRecursive(0, Nil)

    if (seq.nonEmpty) {
      val altPattern = AltPattern(seq)
      Some(altPattern, seqEndIndex)
    } else {
      //nothing parsed, no characters eaten from input stream
      None
    }
  }
}

/**
  * AltPattern (also known as an "alt", short for "alternation") is a
  * set of ConcPatterns. A pattern expresses multiple alternate possibilities.
  * When written out as a regex, these would separated by pipes. A pattern
  * containing no possibilities is possible and represents a regular expression
  * matching no strings whatsoever (there is no conventional string form for
  * this).
  * e.g. "abc|def(ghi|jkl)" is an alt containing two ConcPatterns: "abc" and
  * "def(ghi|jkl)". The latter is a ConcPattern containing four MultPatterns: "d", "e", "f"
  * and "(ghi|jkl)". The latter in turn is a MultPattern consisting of an upper bound
  * 1, a lower bound 1, and a multiplicand which is a new subpattern, "ghi|jkl".
  * This new subpattern again consists of two ConcPatterns: "ghi" and "jkl".
  */
final case class AltPattern(concs: Set[ConcPattern]) extends RegexPattern {

  override lazy val alphabet: Set[Char] = concs.flatMap(_.alphabet) + Fsm.anythingElse

  override def hashCode: Int = concs.hashCode

  override def equivalent(that: RegexPattern): Boolean = that match {
    case thatAlt: AltPattern => concs == thatAlt.concs
    case _                   => false
  }

  override def multiply(multiplier: Multiplier): MultPattern = MultPattern(this, multiplier)

  override def isEmpty: Boolean = concs.forall(_.isEmpty)

  override def concatenate(that: RegexPattern): ConcPattern =
    MultPattern(this, Multiplier.presetOne) + that

  override def intersection(that: RegexPattern): RegexPattern = {
    //A deceptively simple method for an astoundingly difficult operation
    val unionAlphabet = alphabet ++ that.alphabet
    //Which means that we can build finite state machines sharing that alphabet
    val combinedFsm = toFsm(unionAlphabet) & that.toFsm(unionAlphabet)
    RegexPattern.fromFsm(combinedFsm)
  }

  override def union(that: RegexPattern): AltPattern = {
    val thatAlt: AltPattern = that match {
      case altPattern: AltPattern => altPattern
      case charsPattern: CharClassPattern =>
        AltPattern(ConcPattern(MultPattern(charsPattern, Multiplier.presetOne)))
      case concPattern: ConcPattern => AltPattern(concPattern)
      case multPattern: MultPattern => AltPattern(ConcPattern(multPattern))
    }

    AltPattern(concs ++ thatAlt.concs)
  }

  override def toFsm(alphabet: Option[Set[Char]]): Fsm = {
    val actualAlphabet = alphabet.getOrElse(this.alphabet)
    concs.foldLeft(Fsm.nullFsm(actualAlphabet))(_ | _.toFsm(actualAlphabet))
  }

  override def reduced: RegexPattern = throw new NotImplementedError("TODO")

  override def reversed: AltPattern = AltPattern(concs.map(_.reversed))
}

object MultPattern extends ParsedPattern {
  def tryParse(str: CharSequence): Option[(MultPattern, Int)] = {
    //matches single charclass or unnamed group (...)
    def matchMultiplicand(startIndex: Int): Option[(RegexPattern, Int)] =
      if (startIndex < str.length) {
        str.charAt(startIndex) match {
          case '(' => {
            //parse unnamed group
            val parsedAltPattern =
              AltPattern.tryParse(str.subSequence(startIndex + 1, str.length))
            parsedAltPattern.flatMap {
              case (altPattern, altInnerEndIndex) => {
                val altEndIndex = startIndex + 1 + altInnerEndIndex
                if ((altEndIndex < str.length) && (str.charAt(altEndIndex) == ')')) {
                  //we found closing bracket, group finished
                  Some((altPattern.asInstanceOf[RegexPattern], altEndIndex + 1))
                } else {
                  None
                }
              }
            }
          }
          case _ => CharClassPattern.tryParse(str.subSequence(startIndex, str.length))
        }
      } else {
        None
      }

    matchMultiplicand(0).flatMap {
      case (multiplicandPattern, multiplicandEndIndex) =>
        Multiplier
          .tryParse(str.subSequence(multiplicandEndIndex, str.length))
          .flatMap {
            case (multiplier, multiplierEndIndex) =>
              Some(MultPattern(multiplicandPattern, multiplier),
                   multiplicandEndIndex + multiplierEndIndex)
          }
    }
  }
}

/**
  * A MultPattern is a combination of a multiplicand with
  * a multiplier (a min and a max). The vast majority of characters in regular
  * expressions occur without a specific multiplier, which is implicitly
  * equivalent to a min of 1 and a max of 1, but many more have explicit
  * multipliers like "*" (min = 0, max = inf) and so on.
  * e.g. a, b{2}, c?, d*, [efg]{2,5}, f{2,}, (anysubpattern)+, .*, and so on
  */
final case class MultPattern(multiplicand: RegexPattern, multiplier: Multiplier)
    extends RegexPattern {

  override lazy val alphabet: Set[Char] = multiplicand.alphabet + Fsm.anythingElse

  override def hashCode: Int = multiplicand.hashCode ^ multiplier.hashCode

  override def equivalent(that: RegexPattern): Boolean = that match {
    case m: MultPattern => (multiplier == m.multiplier) && multiplicand.equivalent(m.multiplicand)
    case _              => false
  }

  override def multiply(nextMultiplier: Multiplier): MultPattern =
    if (nextMultiplier.isOne)
      this
    else if (multiplier.canMultiplyBy(nextMultiplier))
      MultPattern(multiplicand, multiplier * nextMultiplier)
    else
      MultPattern(AltPattern(ConcPattern(this)), multiplier)

  override def concatenate(that: RegexPattern): ConcPattern = ConcPattern(this) + that

  override def union(that: RegexPattern): RegexPattern = ConcPattern(this) | that

  override def intersection(that: RegexPattern): RegexPattern = {
    val thatMult = that match {
      case thatMult: MultPattern      => thatMult
      case otherPattern: RegexPattern => MultPattern(otherPattern, Multiplier.presetOne)
    }

    if ((thatMult.multiplicand == this.multiplicand) && this.multiplier.canIntersect(
          thatMult.multiplier)) {
      MultPattern(this.multiplicand, this.multiplier & thatMult.multiplier)
    } else {
      ConcPattern(this) & that
    }
  }

  override def isEmpty: Boolean = multiplicand.isEmpty || (multiplier.max.getOrElse(1) < 1)

  override def reversed: MultPattern = MultPattern(multiplicand.reversed, multiplier)

  override def toFsm(alphabet: Option[Set[Char]]): Fsm = {
    val actualAlphabet = alphabet.getOrElse(this.alphabet)

    val startFsm = multiplicand.toFsm(actualAlphabet)
    //accepts e.g. "ab"
    val mandatoryFsm = startFsm * multiplier.mandatory.getOrElse(0)
    //unlimited additional copies
    val optionalFsm = if (multiplier.optional == Multiplier.Inf) {
      startFsm.star
    } else {
      (Fsm.epsilonFsm(actualAlphabet) | startFsm) * multiplier.optional.get
    }

    mandatoryFsm + optionalFsm
  }

  override def reduced: RegexPattern = throw new NotImplementedError("TODO")

  /**
    * Return the common part of these two MultPatterns. This is the largest MultPattern
    * which can be safely subtracted from both the originals.
    * The multiplier on this MultPattern could be zero: this is the case if,
    * for example, the multiplicands disagree.
    */
  def common(that: MultPattern): MultPattern =
    if (multiplicand == that.multiplicand) {
      MultPattern(multiplicand, multiplier.common(that.multiplier))
    } else {
      //Multiplicands disagree, no common part at all.
      MultPattern(RegexPattern.nothing, Multiplier.presetZero)
    }
}
