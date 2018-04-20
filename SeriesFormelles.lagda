\begin{code}
import Level

open import Data.Empty
open import Data.Unit
open import Data.Nat
open import Data.Nat.Properties using (+-suc)
open import Data.List
open import Data.List.Properties
open import Data.List.Categorical

open import Category.Monad
open RawMonad (monad {Level.zero})  -- List monad/applicative

open import Relation.Binary.PropositionalEquality using ( _≡_; refl; sym; cong; module ≡-Reasoning )
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
ZERO _ = 0

ONE  : GF
ONE 0  = 1
ONE _  = 0

X : GF
X 1  = 1
X _  = 0
\end{code}
%</PrimGF>

%<*SumGF>
\begin{code}
_⊕_ : GF → GF → GF
(f ⊕ g) n = f n + g n
\end{code}
%</SumGF>

%<*ProdGF>
\begin{code}
_⊙_ : GF → GF → GF
(f ⊙ g) n = sum (applyUpTo (λ k → f k * g (n ∸ k)) (suc n))
\end{code}
%</ProdGF>

\begin{code}
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

convolve : {A : Set} → (n : ℕ) → ((j k : ℕ) → (j + k ≡ n) → A) → List A
convolve {A} n f = go 0 n refl
  where
    go : (j k : ℕ) → (j + k ≡ n) → List A
    go _ 0       _    = []
    go j (suc k) refl = f j (suc k) refl ∷ go (suc j) k (sym (+-suc j k))

mutual
  mkPairs : (F G : U) → {n : ℕ} → (j k : ℕ) → j + k ≡ n → List (Struct (F ⊡ G) n)
  mkPairs F G j k refl = pair <$> enumerate F j ⊛ enumerate G k

  enumerate : (F : U) → ((n : ℕ) → List (Struct F n))
  enumerate 𝟘 _             = []
  enumerate 𝟙 zero          = [ unit ]
  enumerate 𝟙 (suc _)       = []
  enumerate 𝕏 zero          = []
  enumerate 𝕏 (suc zero)    = [ atom ]
  enumerate 𝕏 (suc (suc _)) = []
  enumerate (F ⊞ G) n       = map inl (enumerate F n) ++ map inr (enumerate G n)
  enumerate (F ⊡ G) n       = concat (convolve n (mkPairs F G))

length-concat : ∀ {a} {A : Set a} (xs : List (List A))
              → length (concat xs) ≡ sum (map length xs)
length-concat []         = refl
length-concat (xs ∷ xss) =
  length (concat (xs ∷ xss))
                                          ≡⟨⟩
  length (xs ++ concat xss)
                                          ≡⟨ length-++ xs ⟩
  length xs + length (concat xss)
                                          ≡⟨ cong (λ r → length xs + r) (length-concat xss) ⟩
  length xs + sum (map length xss)
                                          ≡⟨⟩
  sum (length xs ∷ map length xss)
                                          ≡⟨⟩
  sum (map length (xs ∷ xss))
  ∎
  where
    open ≡-Reasoning

CardCorrect : (F : U) → (n : ℕ) → length (enumerate F n) ≡ Card F n
CardCorrect 𝟘 n             = refl
CardCorrect 𝟙 zero          = refl
CardCorrect 𝟙 (suc n)       = refl
CardCorrect 𝕏 zero          = refl
CardCorrect 𝕏 (suc zero)    = refl
CardCorrect 𝕏 (suc (suc n)) = refl
CardCorrect (F ⊞ G) n =
  length (map inl (enumerate F n) ++ map inr (enumerate G n))
                                          ≡⟨ length-++ (map inl (enumerate F n)) ⟩
  length (map inl (enumerate F n)) + length (map inr (enumerate G n))
                                          ≡⟨ cong (λ r → r + length (map inr (enumerate G n)))
                                                  (length-map _ (enumerate F n))
                                           ⟩
  length (enumerate F n) + length (map inr (enumerate G n))
                                          ≡⟨ cong (λ r → length (enumerate F n) + r)
                                                  (length-map _ (enumerate G n))
                                           ⟩
  length (enumerate F n) + length (enumerate G n)
                                          ≡⟨ cong (λ r → r + length (enumerate G n))
                                                  (CardCorrect F n)
                                           ⟩
  Card F n + length (enumerate G n)
                                          ≡⟨ cong (λ r → Card F n + r)
                                                  (CardCorrect G n)
                                           ⟩
  Card F n + Card G n
                                          ≡⟨⟩
  (Card F ⊕ Card G) n
  ∎
  where
    open ≡-Reasoning

CardCorrect (F ⊡ G) n =
  length (concat (convolve n (mkPairs F G)))
                                          ≡⟨ length-concat ((convolve n (mkPairs F G))) ⟩
  sum (map length (convolve n (mkPairs F G)))
                                          ≡⟨ {!!} ⟩
  (Card F ⊙ Card G) n
  ∎
  where
    open ≡-Reasoning

\end{code}
