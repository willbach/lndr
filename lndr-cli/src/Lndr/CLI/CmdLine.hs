{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lndr.CLI.CmdLine (
      LndrCmd(..)
    , programModes
    ) where

import           Data.Text                       (Text)
import           System.Console.CmdArgs          hiding (def)
import           System.Console.CmdArgs.Explicit (HelpFormat (..), helpText,
                                                  modeEmpty)

data LndrCmd = Transactions
             | Pending
             | RejectPending
             | Lend { friend :: Text
                    , amount :: Integer
                    , memo   :: Text
                    }
             | Borrow { friend :: Text
                      , amount :: Integer
                      , memo   :: Text
                      }
             | Nick { nick :: Text }
             | SearchNick { nick :: Text }
             | GetNonce { friend :: Text }
             | AddFriend { friend :: Text }
             | RemoveFriend { friend :: Text }
             | SetPhoto { photoPath :: String }
             | Info
             | Unsubmitted
             | Settlements
             | LndrConfig
             deriving (Show, Data, Typeable)


programModes = modes [ Transactions &= help "list all transactions involving default user in default Lndr UCAC"
                     , Pending &= help "list all pending transactions"
                     , RejectPending &= "start interactive credit rejection procss"
                     , Lend "0x8c12aab5ffbe1f95b890f60832002f3bbc6fa4cf"
                            123
                            "default"
                            &= help "submit a unilateral transaction as a creditor"
                     , Borrow "0x198e13017d2333712bd942d8b028610b95c363da"
                              123
                              "default"
                              &= help "submit a unilateral transaction as a debtor"
                     , Nick "aupiff" &= help "set a nickname for default user"
                     , SearchNick "aupiff" &= help "find address for a corresponding nickname"
                     , GetNonce "0x198e13017d2333712bd942d8b028610b95c363da"
                     , AddFriend "0x198e13017d2333712bd942d8b028610b95c363da"
                     , RemoveFriend "0x198e13017d2333712bd942d8b028610b95c363da"
                     , SetPhoto "image.jpeg"
                     , Unsubmitted &= help "prints txs that are in lndr db but not yet on the blockchain"
                     , Info &= help "prints config, nick, and friends"
                     , PendingSettlements &= help "list all pending settlements"
                     , LndrConfig &= help "prints config endpoint response"
                     ] &= help "Lend and borrow money.\n Server URL, default user, and default ucac must be indicated in configuration file." &= program "lndr" &= summary "lndr v0.1"
