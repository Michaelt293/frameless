package frameless

import shapeless._

object InjectionEnum {
  implicit val cnilInjectionEnum: Injection[CNil, Int] =
    Injection(
      _ => throw new Exception("Impossible"),
      _ =>
        throw new IllegalArgumentException(
          "Cannot construct a value of type CNil: index out of bounds"
        )
    )

  implicit def coproductInjectionEnum[H, T <: Coproduct](
    implicit
    gen: Generic.Aux[H, HNil],
    tInjectionEnum: Injection[T, Int]
    ): Injection[H :+: T, Int] = {
    Injection(
      {
        case Inl(_) => 0
        case Inr(t) => 1 + tInjectionEnum.apply(t)
      },
      index =>
        if (index == 0)
          Inl(gen.from(HNil))
        else
          Inr(tInjectionEnum.invert(index - 1))
    )
  }

  implicit def genericInjectionEnum[A, R](
    implicit
    gen: Generic.Aux[A, R],
    rInjectionEnum: Injection[R, Int]
    ): Injection[A, Int] =
    Injection(
      value =>
        rInjectionEnum.apply(gen.to(value)),
      name =>
        gen.from(rInjectionEnum.invert(name))
    )
}
