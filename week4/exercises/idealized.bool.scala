package idealized.scala.Boolean

/*
  Less than, assuming False < True:
    False < False => False
    False < True => True
    True < False => False
    True < True => False

  Therefore, < is equivalent to !self && other

  Designed here to short-circuit if possible instead of evaluating the RHS,
  despite the fact that it's not so in the lesson's solution to this problem.
 */

def < (x: => Boolean): Boolean = ifThenElse(false, x)
