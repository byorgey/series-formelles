\begin{code}
open import Data.Empty
open import Data.Unit
open import Data.Nat
open import Data.List
open import Data.List.Properties

open import Relation.Binary.PropositionalEquality using ( _≡_; refl )
\end{code}

%<*GF>
\begin{code}
GF : Set
GF = ℕ → ℕ
\end{code}
%</GF>

%<*PrimGF>
\begin{code}
ZERO : GF
ZERO = λ _ → 0

ONE  : GF
ONE 0 = 1
ONE _ = 0

X : GF
X 1 = 1
X _ = 0
\end{code}
%</PrimGF>

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

data Struct : U → ℕ → Set where
  unit : Struct 𝟙 0
  atom : Struct 𝕏 1
  inl  : {F G : U} {n : ℕ} → Struct F n → Struct (F ⊞ G) n
  inr  : {F G : U} {n : ℕ} → Struct G n → Struct (F ⊞ G) n
  pair : {F G : U} {m n : ℕ} → Struct F m → Struct G n → Struct (F ⊡ G) (m + n)

enumerate : (F : U) → ((n : ℕ) → List (Struct F n))
enumerate 𝟘 _             = []
enumerate 𝟙 zero          = [ unit ]
enumerate 𝟙 (suc _)       = []
enumerate 𝕏 zero          = []
enumerate 𝕏 (suc zero)    = [ atom ]
enumerate 𝕏 (suc (suc _)) = []
enumerate (F ⊞ G) n       = map inl (enumerate F n) ++ map inr (enumerate G n)
enumerate (F ⊡ G) n       = {!!}

CardCorrect : (F : U) → (n : ℕ) → length (enumerate F n) ≡ Card F n
CardCorrect 𝟘 n             = refl
CardCorrect 𝟙 zero          = refl
CardCorrect 𝟙 (suc n)       = refl
CardCorrect 𝕏 zero          = refl
CardCorrect 𝕏 (suc zero)    = refl
CardCorrect 𝕏 (suc (suc n)) = refl
CardCorrect (F ⊞ G) n = {!length-++!}
CardCorrect (F ⊡ G) n = {!!}
