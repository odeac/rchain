package coop.rchain.storage.regex

import org.scalatest._

class RegexPatternUnitTests extends FlatSpec with Matchers {

  "CharClassPattern equality" should "pass checks" in {
    assert(CharClassPattern("a") == CharClassPattern("a"))
    assert(~(~CharClassPattern("a")) == CharClassPattern("a"))
    assert(CharClassPattern("a") == ~(~CharClassPattern("a")))
    assert(~CharClassPattern("a") == ~CharClassPattern("a"))
    assert(~CharClassPattern("a") != CharClassPattern("a"))
    assert(CharClassPattern("ab") == CharClassPattern("ba"))
  }

  "CharClassPattern union" should "operation checks" in {
    assert((CharClassPattern("ab") | CharClassPattern("bc")) == CharClassPattern("abc"))
    assert((CharClassPattern("ab") | ~CharClassPattern("bc")) == ~CharClassPattern("c"))
    assert((~CharClassPattern("ab") | CharClassPattern("bc")) == ~CharClassPattern("a"))
    assert((~CharClassPattern("ab") | ~CharClassPattern("bc")) == ~CharClassPattern("b"))
  }

  "CharClassPattern intersection" should "operation checks" in {
    assert((CharClassPattern("ab") & CharClassPattern("bc")) == CharClassPattern("b"))
    assert((CharClassPattern("ab") & ~CharClassPattern("bc")) == CharClassPattern("a"))
    assert((~CharClassPattern("ab") & CharClassPattern("bc")) == CharClassPattern("c"))
    assert((~CharClassPattern("ab") & ~CharClassPattern("bc")) == ~CharClassPattern("abc"))
  }

  "CharClassPattern isEmpty" should "work well" in {
    assert(CharClassPattern("").isEmpty)
    assert(!(~CharClassPattern("")).isEmpty)
  }

  "CharClassPattern multiplication" should "pass checks" in {
    assert(CharClassPattern("a") * 1 == MultPattern(CharClassPattern("a"), Multiplier.presetOne))
    assert((CharClassPattern("a") * Multiplier(Some(1), Some(2))) != CharClassPattern("a"))
  }

  "CharClassPattern Fsm" should "accept test patterns" in {
    val notA = (~CharClassPattern("a")).toFsm()
    assert(notA.alphabet == Set('a', Fsm.anythingElse))
    assert(notA.accepts("b"))
    assert(notA.accepts(Fsm.anythingElse.toString))
  }

  "CharClassPattern tryParse" should "parse all char classes" in {
    assert(CharClassPattern.parse("\\x41").contains(CharClassPattern("A")))
    assert(CharClassPattern.parse("\\u0041").contains(CharClassPattern("A")))

    assert(CharClassPattern.parse("\\[").contains(CharClassPattern("[")))
    assert(CharClassPattern.parse("\\t").contains(CharClassPattern("\t")))
    assert(CharClassPattern.parse("[\\t]").contains(CharClassPattern("\t")))
    assert(CharClassPattern.parse("\\Z").contains(CharClassPattern("Z")))
    assert(CharClassPattern.parse("[\\Z]").contains(CharClassPattern("Z")))
    assert(
      CharClassPattern.parse("[^\\t\\[]").contains(CharClassPattern("\t[", negateCharSet = true)))

    assert(CharClassPattern.parse("a").contains(CharClassPattern("a")))
    assert(CharClassPattern.parse("\\s").contains(CharClassPattern("\t\n\11\f\r ")))
    assert(
      CharClassPattern
        .parse("\\S")
        .contains(CharClassPattern("\t\n\11\f\r ", negateCharSet = true)))
    assert(CharClassPattern.parse("\\d").contains(CharClassPattern("0123456789")))
    assert(
      CharClassPattern
        .parse("\\D")
        .contains(CharClassPattern("0123456789", negateCharSet = true)))
    assert(
      CharClassPattern
        .parse("\\w")
        .contains(
          CharClassPattern("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")))
    assert(
      CharClassPattern
        .parse("\\W")
        .contains(
          CharClassPattern("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz",
                           negateCharSet = true)))
    assert(CharClassPattern.parse(".").contains(~CharClassPattern("")))
    assert(CharClassPattern.parse("[abc]").contains(CharClassPattern("abc")))
    assert(
      CharClassPattern.parse("[^abc]").contains(CharClassPattern("abc", negateCharSet = true)))

    assert(CharClassPattern.parse("[]").contains(CharClassPattern("")))
    assert(CharClassPattern.parse("[\\x41]").contains(CharClassPattern("A")))
    assert(CharClassPattern.parse("[\\x41-\\x44]").contains(CharClassPattern("ABCD")))

    assert(
      CharClassPattern.parse("[^\\x41]").contains(CharClassPattern("A", negateCharSet = true)))
    assert(
      CharClassPattern
        .parse("[^\\x41-\\x44]")
        .contains(CharClassPattern("ABCD", negateCharSet = true)))

    assert(CharClassPattern.parse("[\\u0041]").contains(CharClassPattern("A")))
    assert(CharClassPattern.parse("[\\u0041-\\u0044]").contains(CharClassPattern("ABCD")))

    assert(
      CharClassPattern.parse("[^\\u0041]").contains(CharClassPattern("A", negateCharSet = true)))
    assert(
      CharClassPattern
        .parse("[^\\u0041-\\u0044]")
        .contains(CharClassPattern("ABCD", negateCharSet = true)))
  }

  "CharClassPattern parse" should "return None on invalid patterns" in {
    assert(CharClassPattern.parse("\\x4").isEmpty)
    assert(CharClassPattern.parse("\\u004").isEmpty)
    assert(CharClassPattern.parse("\\").isEmpty)
    assert(CharClassPattern.parse("[").isEmpty)
    assert(CharClassPattern.parse("[a-").isEmpty)
    assert(CharClassPattern.parse("[^").isEmpty)
    assert(CharClassPattern.parse("[^\\").isEmpty)
    assert(CharClassPattern.parse("[").isEmpty)
    assert(CharClassPattern.parse("[^a").isEmpty)
    assert(CharClassPattern.parse("[^a-").isEmpty)
    assert(CharClassPattern.parse("[^a--b]").isEmpty)
    assert(CharClassPattern.parse("[^a-]").isEmpty)
    assert(CharClassPattern.parse("[^\\x3]").isEmpty)
    assert(CharClassPattern.parse("[^\\u003]").isEmpty)
  }

  "MultPattern parse" should "accept simple mult sequences" in {
    assert(
      MultPattern.parse("a").contains(MultPattern(CharClassPattern("a"), Multiplier.presetOne)))
    assert(
      MultPattern.parse("a*").contains(MultPattern(CharClassPattern("a"), Multiplier.presetStar)))
    assert(
      MultPattern
        .parse("a?")
        .contains(MultPattern(CharClassPattern("a"), Multiplier.presetQuestion)))
    assert(
      MultPattern.parse("a+").contains(MultPattern(CharClassPattern("a"), Multiplier.presetPlus)))
    assert(
      MultPattern
        .parse("a{3,5}")
        .contains(MultPattern(CharClassPattern("a"), Multiplier(Some(3), Some(5)))))
    assert(
      MultPattern
        .parse("a{3,}")
        .contains(MultPattern(CharClassPattern("a"), Multiplier(Some(3), Multiplier.Inf))))
  }

  "MultPattern parse" should "return None on invalid patterns" in {
    assert(MultPattern.parse("(a").isEmpty)
    assert(MultPattern.parse("a{}").isEmpty)
    assert(MultPattern.parse("a{3").isEmpty)
    assert(MultPattern.parse("a{3,").isEmpty)
    assert(MultPattern.parse("a{,4}").isEmpty)
  }

  "MultPattern multiply" should "work as expected" in {
    val a = MultPattern(CharClassPattern("a"), Multiplier.presetOne)
    assert(a == a.multiply(Multiplier.presetOne))
    assert(
      MultPattern(CharClassPattern("a"), Multiplier(Some(2), Some(2))) == a.multiply(
        Multiplier(Some(2), Some(2))))
    assert(
      MultPattern(CharClassPattern("a"), Multiplier.presetPlus).multiply(
        Multiplier(Some(3), Some(4))) == a.multiply(Multiplier(Some(3), Multiplier.Inf)))
  }

  "RegexPattern parse" should "parse groups" in {
    assert(
      MultPattern
        .tryParse("(a)")
        .contains(
          (MultPattern(AltPattern(ConcPattern(CharClassPattern("a"))), Multiplier.presetOne), 3)))

    val ain = MultPattern.parse("((a))").get.toFsm()
    assert(ain.accepts("a"))

    val ab = RegexPattern.parse("(a)b").get
    assert(
      ab == AltPattern(
        ConcPattern(
          MultPattern(AltPattern(ConcPattern(CharClassPattern("a"))), Multiplier.presetOne) ::
            MultPattern(CharClassPattern("b"), Multiplier.presetOne) :: Nil)))

    assert(ab.toFsm().accepts("ab"))

    val a2b = RegexPattern.parse("((a))b").get.toFsm()
    assert(a2b.accepts("ab"))

    val abc = RegexPattern.parse("((a)(b*))c").get.toFsm()
    assert(abc.accepts("abbbc"))
  }

  "MultPattern common operation" should "work as expected" in {
    val aStar = MultPattern.parse("a*").get.asInstanceOf[MultPattern]
    val aPlus = MultPattern.parse("a+").get.asInstanceOf[MultPattern]
    val aOne = MultPattern.parse("a").get.asInstanceOf[MultPattern]

    assert(aStar.common(aPlus) == aStar)
  }

  "ConcPattern parse" should "parse sequences" in {
    assert(
      ConcPattern
        .parse("a")
        .contains(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))))

    assert(
      ConcPattern
        .parse("ab")
        .contains(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)
          :: MultPattern(CharClassPattern("b"), Multiplier.presetOne) :: Nil)))

    assert(
      ConcPattern
        .parse("abc")
        .contains(
          ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)
            :: MultPattern(CharClassPattern("b"), Multiplier.presetOne)
            :: MultPattern(CharClassPattern("c"), Multiplier.presetOne) :: Nil)))
  }

  "ConcPattern parse" should "return None on invalid sequences" in {
    assert(ConcPattern.parse("").isEmpty)
    assert(ConcPattern.parse("\\").isEmpty)
  }

  "ConcPattern equality" should "work well on rare cases" in {
    assert(ConcPattern(Nil) == ConcPattern(Nil))
    //different pattern types comparison shouldn't fail
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) != MultPattern(
        CharClassPattern("a"),
        Multiplier.presetOne))
  }

  "AltPattern tryParse" should "parse alt sequences" in {
    assert(
      RegexPattern
        .parse("a|b")
        .contains(
          AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) ::
            ConcPattern(MultPattern(CharClassPattern("b"), Multiplier.presetOne)) :: Nil)))

    assert(
      RegexPattern
        .parse("a|b")
        .contains(
          AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) ::
            ConcPattern(MultPattern(CharClassPattern("b"), Multiplier.presetOne)) :: Nil)))

    assert(
      RegexPattern
        .parse("a?b")
        .contains(
          AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetQuestion) ::
            MultPattern(CharClassPattern("b"), Multiplier.presetOne) :: Nil))))

    assert(
      RegexPattern
        .parse("a?b{3,}")
        .contains(
          AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetQuestion) ::
            MultPattern(CharClassPattern("b"), Multiplier(Some(3), Multiplier.Inf)) :: Nil))))

    assert(RegexPattern.parse("ac*").nonEmpty)

    assert(RegexPattern.parse("b{3,4}c").nonEmpty)

    assert(RegexPattern.parse("b{3,}c").nonEmpty)

    assert(
      RegexPattern
        .parse("a?b{3,}c*")
        .contains(
          AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetQuestion) ::
            MultPattern(CharClassPattern("b"), Multiplier(Some(3), Multiplier.Inf)) ::
            MultPattern(CharClassPattern("c"), Multiplier(Some(0), Multiplier.Inf)) :: Nil))))

    assert(
      RegexPattern
        .parse("a?b{3,}c*")
        .contains(
          AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetQuestion) ::
            MultPattern(CharClassPattern("b"), Multiplier(Some(3), Multiplier.Inf)) ::
            MultPattern(CharClassPattern("c"), Multiplier(Some(0), Multiplier.Inf)) :: Nil))))
  }

  "AltPattern" should "handle union operations" in {
    assert(
      (ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetQuestion)) |
        ConcPattern(MultPattern(CharClassPattern("b"), Multiplier.presetQuestion))) == AltPattern(
        ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetQuestion)) :: ConcPattern(
          MultPattern(CharClassPattern("b"), Multiplier.presetQuestion)) :: Nil
      ))
  }

  "Empty" should "work for all char classes" in {
    assert(CharClassPattern("").isEmpty)
    assert(ConcPattern().isEmpty)
    assert(AltPattern(Nil).isEmpty)
    assert(MultPattern(CharClassPattern(""), Multiplier.presetOne).isEmpty)
    assert(MultPattern(CharClassPattern("a"), Multiplier.presetZero).isEmpty)
  }

  "MultPattern" should "produce good Fsm" in {
    val a = CharClassPattern("a")
    val a1 = a * 1
    assert(a1.accepts("a"))
    assert(!a1.accepts("b"))
    assert(!a1.accepts("aa"))

    val a2 = a * 2
    assert(!a2.accepts("a"))
    assert(!a2.accepts("b"))
    assert(a2.accepts("aa"))
    assert(!a2.accepts("aaa"))

    val aQuest = a * Multiplier.presetQuestion
    assert(aQuest.accepts(""))
    assert(aQuest.accepts("a"))
    assert(!aQuest.accepts("b"))
    assert(!aQuest.accepts("aa"))

    val aStar = a * Multiplier.presetStar
    assert(aStar.accepts(""))
    assert(aStar.accepts("a"))
    assert(!aStar.accepts("b"))
    assert(aStar.accepts("aa"))
    assert(aStar.accepts("aaaaaaaaaaaaaaaaaaa"))

    val aPlus = a * Multiplier.presetPlus
    assert(!aPlus.accepts(""))
    assert(aPlus.accepts("a"))
    assert(!aPlus.accepts("b"))
    assert(aStar.accepts("aa"))
    assert(aStar.accepts("aaaaaaaaaaaaaaaaaaa"))

    val aZero = a * Multiplier.presetZero
    assert(aZero.accepts(""))
    assert(!aZero.accepts("a"))
    assert(!aZero.accepts("b"))
  }

  "AltPattern" should "pass equality check" in {}

  "reverse" should "work for all pattern types" in {
    assert(CharClassPattern("a").reversed == CharClassPattern("a"))

    val aOrB = AltPattern(CharClassPattern("a"), CharClassPattern("b"))
    val bOrA = AltPattern(CharClassPattern("b"), CharClassPattern("a"))

    assert(aOrB.reversed == aOrB)
    assert(aOrB.reversed == bOrA)
    assert(aOrB.reversed == bOrA.reversed)

    val ab = ConcPattern(CharClassPattern("a"), CharClassPattern("b"))
    val ba = ConcPattern(CharClassPattern("b"), CharClassPattern("a"))

    assert(ab.reversed == ba)
    assert(ba.reversed == ab)

    val aa = CharClassPattern("a") * 2
    assert(aa.reversed == aa)
  }

  "AltPattern" should "produce good Fsm" in {
    val fsm = AltPattern(CharClassPattern("a"), CharClassPattern("b")).toFsm()
    assert(fsm.accepts("a"))
    assert(fsm.accepts("b"))
    assert(!fsm.accepts("c"))
    assert(!fsm.accepts("aa"))
    assert(!fsm.accepts("ab"))
    assert(!fsm.accepts("ba"))
    assert(!fsm.accepts("bb"))
    assert(!fsm.accepts("a" + Fsm.anythingElse))
    assert(!fsm.accepts("b" + Fsm.anythingElse))
    assert(!fsm.accepts(Fsm.anythingElse + "b"))
    assert(!fsm.accepts(Fsm.anythingElse + "a"))
    assert(!fsm.accepts("" + Fsm.anythingElse + Fsm.anythingElse))
  }

  "ConcPattern" should "produce good Fsm" in {
    val fsm =
      ConcPattern(CharClassPattern("a"), CharClassPattern("a", negateCharSet = true)).toFsm()
    assert(fsm.states.size == 3)
    assert(!fsm.accepts("a"))
    assert(!fsm.accepts("b"))
    assert(!fsm.accepts("aa"))
    assert(fsm.accepts("ab"))
    assert(fsm.accepts("a" + Fsm.anythingElse))
    assert(!fsm.accepts("ba"))
    assert(!fsm.accepts("bb"))
  }

  "MultPattern" should "pass equality check" in {
    assert(
      MultPattern(CharClassPattern("a"), Multiplier.presetOne)
        == MultPattern(CharClassPattern("a"), Multiplier.presetOne))
    assert(
      MultPattern(CharClassPattern("a"), Multiplier.presetOne)
        != MultPattern(CharClassPattern("b"), Multiplier.presetOne))
    assert(
      MultPattern(CharClassPattern("a"), Multiplier.presetOne)
        != MultPattern(CharClassPattern("a"), Multiplier.presetQuestion))
    assert(
      MultPattern(CharClassPattern("a"), Multiplier.presetOne)
        != MultPattern(CharClassPattern("a"), Multiplier(Some(1), Some(2))))
    assert(MultPattern(CharClassPattern("a"), Multiplier.presetOne) != CharClassPattern("a"))
  }

  "ConcPattern" should "pass equality check" in {
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))
        == ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)))
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))
        != ConcPattern(MultPattern(CharClassPattern("b"), Multiplier.presetOne)))
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))
        != ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetQuestion)))
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))
        != ConcPattern(MultPattern(CharClassPattern("a"), Multiplier(Some(1), Some(2)))))
    assert(
      ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))
        != ConcPattern.presetEmptyString)
  }

  "Nested Patterns" should "pass equality checks" in {
    assert(
      AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) ::
        ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) :: Nil)
        == AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne))))

    assert(
      AltPattern(ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) ::
        ConcPattern(MultPattern(CharClassPattern("b"), Multiplier.presetOne)) :: Nil)
        == AltPattern(ConcPattern(MultPattern(CharClassPattern("b"), Multiplier.presetOne)) ::
          ConcPattern(MultPattern(CharClassPattern("a"), Multiplier.presetOne)) :: Nil))
  }

  "ConcPattern" should "be a result of + operation" in {
    assert(
      RegexPattern
        .parse("ba")
        .contains(AltPattern(MultPattern(CharClassPattern("b"), Multiplier.presetOne) +
          MultPattern(CharClassPattern("a"), Multiplier.presetOne))))
  }
}
