{-# LANGUAGE OverloadedStrings #-}
module Text.ABNF.Canonicalizer (canonicalizeRules) where

import Data.List (partition)
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Text.ABNF.Parser.Types

type RuleMap = Map.Map Identifier Rule

canonicalizeRules :: Identifier -> [Rule] -> Maybe Rule
canonicalizeRules mainRuleIdent rules = do
    let (defs, adds) = partition isAdd rules
        ruleMap  = foldr (\rule@(Rule ident _ _) curMap ->
                              Map.insert ident rule curMap)
                         Map.empty
                         defs
        ruleMap' = foldr (\rule@(Rule ident _ _) curMap ->
                              Map.insertWith mergeRules ident rule curMap)
                         ruleMap
                         adds
    mainRule <- Map.lookup mainRuleIdent ruleMap'
    pure $ inlineRulesRule ruleMap' mainRule --TODO: Catch missing rules here
    where
        isAdd (Rule _ Equals _) = False
        isAdd (Rule _ Adds _) = True

mergeRules :: Rule -> Rule -> Rule
mergeRules (Rule ident Equals left) (Rule ident2 Adds right)
  | ident == ident2 = Rule ident Equals
    ( SumSpec [ ProductSpec [ Repetition (Repeat 1 (Just 1))
                                (GroupElement (Group left))
                            ]
              , ProductSpec [ Repetition (Repeat 1 (Just 1))
                                (GroupElement (Group right))
                            ]
              ]
    ) 
  -- TODO: Print location of error
  | otherwise = error . Text.unpack
      $  "Error while canocicalizing ABNF: Rule " <> ident
      <> " appended (=/) without ever assigning(=)"
mergeRules _ _ = error "Bug in ABNF canonicalizer (mergeRules)!"

inlineRulesRule :: RuleMap -> Rule -> Rule
inlineRulesRule rulemap (Rule ident def spec) =
    Rule ident def (inlineRulesSumSpec rulemap spec)

inlineRulesSumSpec :: RuleMap -> SumSpec -> SumSpec
inlineRulesSumSpec rulemap (SumSpec specs) =
    SumSpec $ inlineRulesProdSpec rulemap <$> specs

inlineRulesProdSpec :: RuleMap -> ProductSpec -> ProductSpec
inlineRulesProdSpec rulemap (ProductSpec reps) =
    ProductSpec $ inlineRulesRepetition rulemap <$> reps

inlineRulesRepetition :: RuleMap -> Repetition -> Repetition
inlineRulesRepetition rulemap (Repetition rep elem) =
    Repetition rep $ inlineRulesElement rulemap elem

inlineRulesElement :: RuleMap -> Element -> Element
inlineRulesElement rulemap oldrule@(RuleElement' ruleName) =
    let rule = Map.lookup ruleName rulemap
    in  maybe oldrule (RuleElement . inlineRulesRule rulemap) rule
inlineRulesElement rulemap (GroupElement grp) =
    GroupElement $ inlineRulesGroup rulemap grp
inlineRulesElement rulemap (OptionElement grp) =
    OptionElement $ inlineRulesGroup rulemap grp
inlineRulesElement _ old = old

inlineRulesGroup :: RuleMap -> Group -> Group
inlineRulesGroup rulemap (Group sumspec) = Group (inlineRulesSumSpec rulemap sumspec)
