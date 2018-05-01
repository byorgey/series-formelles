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

%<*OGF>
\begin{code}
OGF : Set
OGF = ℕ → ℕ
\end{code}
%</OGF>

%<*PrimOGF>
\begin{code}
ZERO : OGF
ZERO _ = 0

ONE  : OGF
ONE 0  = 1
ONE _  = 0

X : OGF
X 1  = 1
X _  = 0
\end{code}
%</PrimOGF>

%<*SumOGF>
\begin{code}
_⊕_ : OGF → OGF → OGF
(f ⊕ g) n = f n + g n
\end{code}
%</SumOGF>

%<*ProdOGF>
\begin{code}
_⊙_ : OGF → OGF → OGF
(f ⊙ g) n = sum (applyUpTo (λ k → f k * g (n ∸ k)) (suc n))
\end{code}
%</ProdOGF>

\begin{code}
data U : Set where
  𝟘   : U
  𝟙   : U
  𝕏   : U
  _⊞_ : U → U → U
  _⊡_ : U → U → U

data Struct : U → Set where
  unit : Struct 𝟙
  atom : Struct 𝕏
  inl  : {F G : U} → Struct F → Struct (F ⊞ G)
  inr  : {F G : U} → Struct G → Struct (F ⊞ G)
  pair : {F G : U} → Struct F → Struct G → Struct (F ⊡ G)

mutual
  mkPairs : (F G : U) → (j k : ℕ) → List (Struct (F ⊡ G))
  mkPairs F G j k = pair <$> enumerate F j ⊛ enumerate G k

  enumerate : (F : U) → ((n : ℕ) → List (Struct F))
  enumerate 𝟘 _             = []
  enumerate 𝟙 zero          = [ unit ]
  enumerate 𝟙 (suc _)       = []
  enumerate 𝕏 zero          = []
  enumerate 𝕏 (suc zero)    = [ atom ]
  enumerate 𝕏 (suc (suc _)) = []
  enumerate (F ⊞ G) n       = map inl (enumerate F n) ++ map inr (enumerate G n)
  enumerate (F ⊡ G) n       = concat (applyUpTo (λ j → mkPairs F G j (n ∸ j)) (suc n))
\end{code}
