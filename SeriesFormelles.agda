open import Data.Nat
open import Data.List

GF : Set
GF = ℕ → ℕ

ZERO : GF
ZERO = λ _ → 0

ONE  : GF
ONE 0 = 1
ONE _ = 0

X : GF
X 1 = 1
X _ = 0

_⊕_ : GF → GF → GF
(f ⊕ g) n = f n + g n

_⊙_ : GF → GF → GF
(f ⊙ g) n = sum (applyUpTo (λ k → f k * g (n ∸ k)) (suc n))

data U : Set where
  𝟘   : U
  𝟙   : U
  𝕏   : U
  _⊞_ : U → U → U
  _⊡_ : U → U → U

Card : U → GF
Card 𝟘        = ZERO
Card 𝟙        = ONE
Card 𝕏        = X
Card (f ⊞ g)  = Card f ⊕ Card g
Card (f ⊡ g)  = Card f ⊙ Card g

