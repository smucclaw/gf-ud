--# -path=../../grammars
-- This grammar is there to test different ways to order the candidate trees.
-- For example, we want to see if one way leads to flatter trees than the other.
abstract Test = MiniLang - [PredVP] ** {
    flags startcat = UDS ;

    cat UDS ;

    -- To test ordering of trees
    fun root_nsubj_obl : VP -> NP -> Adv -> UDS ; -- [the cat]:NP [sleeps]:VP [today]:Adv
    fun root_nsubj     : VP -> NP -> UDS ;        -- [the cat]:NP [sleeps today]:VP

    -- To test lemma/wordform in auxfuns
    fun someSg_Det, somePl_Det, anySg_Det, anyPl_Det : Det ;

    -- To test distance feature
    cat Num ;
    fun ten_Num : Num ;
    fun num2Det : Num -> Det ;

    fun ApposNum : CN -> Num -> CN ; -- Section 10

    -- To test the order of applications based on word order
    -- we need some postmodifier APs
    fun AdvCN : CN -> Adv -> CN ;
    fun PastPartAgentAP : V2 -> NP -> AP ; -- separated by walls
    fun separate_V2 : V2 ;
    fun wall_N : N ;
    fun portion_N : N ;
    fun building_N : N ;
    fun large_A : A ;
    fun each_Det : Det ;

    -- The above seems to work correctly without changes, so need more complex example
    cat Acl ;
    cat Nmod ;
    -- very ad hoc
    fun PastPartAP : VP -> Acl ;
    fun nmod_ : Prep -> NP -> Nmod ;

    -- lexicon for test case
    fun call_V2 : V2 ;
    fun P_NP : NP ;
    fun policy_N : N ;
    fun company_N : N ;
    fun root_acl_nmod : (policy : CN) -> (called_X : Acl) -> (of_company : Nmod) -> UDS ;
    fun root_nmod_acl : (policy : CN) -> (of_company : Nmod) -> (called_X : Acl) -> UDS ;



}