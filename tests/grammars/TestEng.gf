--# -path=../../grammars
concrete TestEng of Test = MiniLangEng - [PredVP] ** open (R=MiniResEng), MiniParadigmsEng, Prelude in {
    lincat
      UDS = {s : Str} ;
    lin
       -- : VP -> NP -> Adv -> UDS ; -- [the cat]:NP [sleeps]:VP [today]:Adv
      root_nsubj_obl vp np adv = root_nsubj (AdvVP vp adv) np ;
      -- : VP -> NP -> UDS ;        -- [the cat]:NP [sleeps today]:VP
      root_nsubj vp np = UseCl TSim PPos (PredVP np vp) ;

      -- : Det
      someSg_Det = {s = "some" ; n = R.Sg} ;
      somePl_Det = {s = "some" ; n = R.Pl} ;
      anySg_Det = {s = "any" ; n = R.Sg} ;
      anyPl_Det = {s = "any" ; n = R.Pl} ;

    -- To test distance feature
    lincat Num = {s : Str ; n : R.Number} ;
    lin ten_Num = {s = "10" ; n = R.Pl} ;
    lin num2Det n = n ;
    lin ApposNum cn num = cn ** {s = \\n => cn.s ! n ++ num.s} ;

    -- To test the order of applications based on word order
    -- we need some postmodifier APs
    -- : V2 -> NP -> AP ; -- separated by walls
    lin PastPartAgentAP v2 np = {
      s = v2.s ! R.PastPart ++ "by" ++ np.s ! R.Acc ;
      isPre = False
    } ;

    lin AdvCN cn adv = cn ** {s = \\nf => cn.s ! nf ++ adv.s} ;
    -- lexicon
    lin separate_V2 = mkV2 "separate" ;
    lin wall_N = mkN "wall" ;
    lin portion_N = mkN "portion" ;
    lin building_N = mkN "building" ;
    lin large_A = mkA "large" ;
    lin each_Det = {s = "each" ; n = R.Sg} ;

    -- Another try at word order
    lincat Acl = SS ;
    lincat Nmod = SS ;
    -- very ad hoc

    -- : VP -> Acl ;
    lin PastPartAP vp = {
      s = vp.verb.s ! R.VF R.PastPart ++ vp.compl
    } ;

    lin nmod_ = MiniLangEng.PrepNP ;

    -- lexicon for test case
    lin call_V2 = mkV2 "call" ;
    lin P_NP = UsePN (mkPN "P") ;
    lin policy_N = mkN "policy" ;
    lin company_N = mkN "company" ;

    -- : (policy : CN) -> (called_X : Acl) -> (of_company : Nmod) -> CN ;
    lin root_acl_nmod policy called_P of_company =
          let cn : CN = AdvCN (AdvCN policy called_P) of_company ;
           in UttNP (MassNP cn) ;

    -- : (policy : CN) -> (of_company : Nmod) -> (called_X : Acl) -> CN ;
    lin root_nmod_acl policy of_company called_P =
          let cn : CN = AdvCN (AdvCN policy of_company) called_P ;
           in UttNP (MassNP cn) ;



}