(* ::Package:: *)

BeginPackage["KryptoNumberTheory`"]
(*Number Theory*)
generateMultiplicativeGroup::usage="Generate multiplicative group of integers modulo n.";
getModularMultiplicativeInverse::usage="Get modular multiplicative invers from a mod n.";
legendreSymbol::usage="Calculates the Legendre symbol using Euler's criterion for
Primes unequal 2.";
(*RSA*)
rsaComputePublicExponent::usage="Generate public RSA exponent.";
rsaComputePrivateExponent::usage="Generate private RSA exponent.";
rsaEncryption::usage="Encrypt message with RSA.";
rsaDecryption::usage="Decrypt ciphertest with RSA.";

Begin["`Private`"]
(*Number Theory*)
generateMultiplicativeGroup[n_]:=(
	gr={};
	For[i=1,i<n,i++,
		If[GCD[i,n]==1,gr=Append[gr,i]];
	];
	Return[gr]
)

getModularMultiplicativeInverse[a_,n_]:=(
	inv = ExtendedGCD[a,n][[2]][[1]];
	If[inv<0,
		inv = inv+n;
	];
	Return[inv];
)

legendreSymbol[a_,p_]:=(
	ls=Mod[a^((p-1)/2),p];
	If[ls>1,ls=-1];
	Return[ls];
)
(*RSA*)
rsaComputePublicExponent[phiN_]:=(
	e=RandomInteger[{1,phiN-1}];
	While[GCD[e,phiN]!=1,e=RandomInteger[{1,phiN-1}]];
	Return[e];
)

rsaComputePrivateExponent[publicExponent_,phiN_]:=(
	Return[getModularMultiplicativeInverse[publicExponent, phiN]];
)

rsaEncryption[message_, publicExponent_,modulus_]:=(
	Return[PowerMod[message,publicExponent,modulus]];
)

rsaDecryption[ciphertext_,privateExponent_,modulus_]:=(
	Return[PowerMod[ciphertext,privateExponent,modulus]];
)

End[]
EndPackage[]









