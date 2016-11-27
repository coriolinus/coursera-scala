package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy(c => c).mapValues(l => l.length).toList.sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    // Merge two sets, summing duplicate keys
    // http://stackoverflow.com/a/7080321/504550
    def merge(map1: Map[Char, Int], map2: Map[Char, Int]): Map[Char, Int] =
      map1 ++ map2.map{ case (k,v) => k -> (v + map1.getOrElse(k,0)) }

    // note that this is kind of ugly--I wish we could have kept Occurrences as a straight map,
    // not a sorted list of pairs. I know it's a relatively cheap transformation between the two
    // forms, but it's not free, and I don't like the frequency with which we have to perform these
    // conversions.
    // In fact, here, we cheat a little by simply not sorting intermediate values, as they're
    // going to be immediately converted back into (unordered) maps. We just preserve the
    // Occurrences invariant of sortedness at the end.
    s.map(wordOccurrences).foldLeft(List[(Char, Int)]())((a: Occurrences, b: Occurrences) =>
      merge(a.toMap, b.toMap).toList).sorted
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy wordOccurrences

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences get(wordOccurrences(word)) match {
    case None => List(word)
    case Some(words) => words
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    /** Returns the list of all Occurrences which are the same as `occurrences`
     * with one item reduced by 1.
     *
     *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
     *
     *    List(
     *      List(('a', 2), ('b', 1)),
     *      List(('a', 1), ('b', 2))
     *    )
     *
     *  Example 2: the subsets of the occurrence list `List(('a', 2), ('b', 1))` are:
     *
     *    List(
     *      List(('a', 1), ('b', 1)),
     *      List(('a', 2))
     *    )
     */
    def minus_one(occurrences: Occurrences): List[Occurrences] = {
      val o_m = occurrences.toMap
      (
        for ((ch, count) <- occurrences) yield o_m + (ch -> (count - 1))
      ).map(_.toList).map(_.filter(_._2 > 0)).map(_.sorted)
    }

    /** All combinations of the occurrences list, except it doesn't include the empty list when
     * the given list is not empty.
     *
     * This generates sets in the hopes that we can reduce the amount of redundant work.
     * If we don't use sets at some point, even in a sequence as simple as `"aab"` we can get
     * to the result `List((a, 1))` by at least two paths: first remove `b` then an `a`, or
     * first remove an `a` then `b`.
     *
     * These redundant records still get generated, which is unavoidable except maybe using
     * super-lazy evaluation, which we're not doing; the efficiency gain is in avoiding building
     * huge result lists with tons of these duplicates.
     */
    def combis(occurrences: Occurrences): Set[Occurrences] = occurrences match {
      case Nil => Set()
      case o => (minus_one(o) flatMap combis).toSet + o
    }

    // combis is _almost_ correct; it just doesn't include the empty list.
    // Also, it generates sets, and we want not to.
    (occurrences match {
      case Nil => Set(List())
      case o => combis(o) + List()
    }).toList
  }



  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   *
   *  This is a prety straightforward modification of the `merge` function defined within
   *  `sentenceOccurrences`.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val x_m = x.toMap
    (
      x_m ++ y.toMap.map{ case (k,v) => k -> (x_m.getOrElse(k,0) - v) }
    ).toList.filter(_._2 > 0).sorted
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if (sentence.length == 0) List(List()) else {
      sentences_from(sentenceOccurrences(sentence))
    }
  }

  /** Get the list of potential words which can be formed from the given occurrences.
   *
   * Return tuples: the word, and the occurrences remaining.
   */
  def word_in(occurrences: Occurrences): List[(Word, Occurrences)] = {
    def flatten_words(ws: List[Word], o: Occurrences): List[(Word, Occurrences)] = ws.map((_, o))

    (for {
      // have to exclude the empty subset to prevent infinite recursion here
      subset <- combinations(occurrences) if subset != Nil
      words <- dictionaryByOccurrences.get(subset)
    } yield flatten_words(words, subtract(occurrences, subset))).flatten
  }

  def sentences_from(occurrences: Occurrences): List[Sentence] = {
    if (occurrences.length == 0) List(List()) else
    for {
      (first, rest) <- word_in(occurrences)
      subsentence <- sentences_from(rest)
    } yield first :: subsentence
  }

}
