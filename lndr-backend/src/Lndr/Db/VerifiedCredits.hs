{-# LANGUAGE OverloadedStrings #-}

module Lndr.Db.VerifiedCredits where

import           Control.Arrow (first)
import           Control.Monad
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Data.Scientific
import           Database.PostgreSQL.Simple
import           Lndr.Types
import           Lndr.Db.Types
import           Lndr.Util
import           Network.Ethereum.Web3

insertCredit :: BilateralCreditRecord -> Connection -> IO Int
insertCredit (BilateralCreditRecord creditRecord creditorSig debtorSig) conn =
    let sql = "INSERT INTO verified_credits (creditor, debtor, amount, memo, nonce, hash, creditor_signature, debtor_signature, ucac, submitter) VALUES (?,?,?,?,?,?,?,?,?,?)"
    in fromIntegral <$> execute conn sql ( creditor creditRecord
                                         , debtor creditRecord
                                         , amount creditRecord
                                         , memo creditRecord
                                         , nonce creditRecord
                                         , hash creditRecord
                                         , creditorSig
                                         , debtorSig
                                         , ucac creditRecord
                                         , submitter creditRecord
                                         )


insertCredits :: [IssueCreditLog] -> Connection -> IO Int
insertCredits creditLogs conn =
    fromIntegral <$> executeMany conn "INSERT INTO verified_credits (creditor, debtor, amount, memo, nonce, hash, creditor_signature, debtor_signature, ucac) VALUES (?,?,?,?,?,?,?,?,?) ON CONFLICT (hash) DO NOTHING" (creditLogToCreditTuple <$> creditLogs)


allCredits :: Connection -> IO [IssueCreditLog]
allCredits conn = query_ conn "SELECT ucac, creditor, debtor, amount, nonce, memo FROM verified_credits"


lookupCreditByAddress :: Address -> Connection -> IO [IssueCreditLog]
lookupCreditByAddress addr conn = query conn "SELECT ucac, creditor, debtor, verified_credits.amount, nonce, memo FROM verified_credits LEFT JOIN settlements ON verified_credits.hash = settlements.hash WHERE (creditor = ? OR debtor = ?) AND settlements.hash IS NULL" (addr, addr)


deleteExpiredSettlementsAndAssociatedCredits :: Connection -> IO ()
deleteExpiredSettlementsAndAssociatedCredits conn = do
    hashes <- fmap fromOnly <$> query_ conn "SELECT hash FROM settlements WHERE created_at < now() - interval '2 days' AND verified = FALSE" :: IO [Text]
    execute conn "DELETE FROM verified_credits WHERE hash IN ?" (Only $ In hashes)
    execute conn "DELETE FROM pending_credits WHERE hash IN ?" (Only $ In hashes)
    void $ execute conn "DELETE FROM settlements WHERE hash IN ?" (Only $ In hashes)


settlementCreditsToVerify :: Connection -> IO [Text]
settlementCreditsToVerify conn = fmap fromOnly <$> query_ conn "SELECT hash FROM settlements WHERE tx_hash IS NOT NULL AND verified = FALSE"


txHashByCreditHash :: Text -> Connection -> IO (Maybe Text)
txHashByCreditHash creditHash conn = fmap fromOnly . join . listToMaybe <$> query conn "SELECT tx_hash FROM settlements WHERE hash = ?" (Only creditHash)


updateSettlementTxHash :: Text -> Text -> Connection -> IO Int
updateSettlementTxHash hash txHash conn = fromIntegral <$> execute conn "UPDATE settlements SET  tx_hash = ? WHERE hash = ?" (txHash, hash)


-- TODO make this reflect reality
lookupSettlementCreditByAddress :: Address -> Connection -> IO [SettlementCreditRecord]
lookupSettlementCreditByAddress addr conn = query conn "SELECT creditor, debtor, verified_credits.amount, memo, creditor, nonce, verified_credits.hash, creditor_signature, ucac, settlements.amount, settlements.currency, settlements.blocknumber, settlements.tx_hash FROM verified_credits JOIN settlements ON verified_credits.hash = settlements.hash WHERE (creditor = ? OR debtor = ?) AND verified = FALSE" (addr, addr)


counterpartiesByAddress :: Address -> Connection -> IO [Address]
counterpartiesByAddress addr conn = fmap fromOnly <$>
    query conn "SELECT creditor FROM verified_credits WHERE debtor = ? UNION SELECT debtor FROM verified_credits WHERE creditor = ?" (addr, addr)


lookupSettlementCreditByHash :: Text -> Connection -> IO (Maybe (CreditRecord, Text, Text, Text))
lookupSettlementCreditByHash hash conn = do
        pairM <- fmap (first (floor :: Rational -> Integer)) . listToMaybe <$> query conn "SELECT amount, tx_hash FROM settlements WHERE hash = ?" (Only hash)
        case pairM of
            Just (settlementAmount, txHash) -> do
                Just (BilateralCreditRecord cr sig1 sig2) <- lookupCreditByHash hash conn
                return $ Just (cr { settlementAmount = Just settlementAmount }, sig1, sig2, txHash)
            Nothing -> return Nothing


lookupCreditByHash :: Text -> Connection -> IO (Maybe BilateralCreditRecord)
lookupCreditByHash hash conn = listToMaybe <$> query conn sql (Only hash)
    where sql = "SELECT creditor, debtor, amount, memo, submitter, nonce, hash, ucac, creditor_signature, debtor_signature FROM verified_credits WHERE hash = ?"


-- Flips verified bit on once a settlement payment has been confirmed
verifyCreditByHash :: Text -> Connection -> IO Int
verifyCreditByHash hash conn = fromIntegral <$> execute conn "UPDATE settlements SET verified = TRUE WHERE hash = ?" (Only hash)


userBalance :: Address -> Connection -> IO Integer
userBalance addr conn = do
    [Only balance] <- query conn "SELECT (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE creditor = ?) - (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE debtor = ?)" (addr, addr) :: IO [Only Scientific]
    return . floor $ balance


twoPartyBalance :: Address -> Address -> Connection -> IO Integer
twoPartyBalance addr counterparty conn = do
    [Only balance] <- query conn "SELECT (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE creditor = ? AND debtor = ?) - (SELECT COALESCE(SUM(amount), 0) FROM verified_credits WHERE creditor = ? AND debtor = ?)" (addr, counterparty, counterparty, addr) :: IO [Only Scientific]
    return . floor $ balance


twoPartyNonce :: Address -> Address -> Connection -> IO Nonce
twoPartyNonce addr counterparty conn = do
    [Only nonce] <- query conn "SELECT COALESCE(MAX(nonce) + 1, 0) FROM verified_credits WHERE (creditor = ? AND debtor = ?) OR (creditor = ? AND debtor = ?)" (addr, counterparty, counterparty, addr) :: IO [Only Scientific]
    return . Nonce . floor $ nonce

-- utility functions

creditLogToCreditTuple :: IssueCreditLog
                       -> (Address, Address, Integer, Text, Integer, Text, Text, Text, Address)
creditLogToCreditTuple cl@(IssueCreditLog ucac creditor debtor amount nonce memo) =
    (creditor, debtor, amount, memo, nonce, hashCreditLog cl, "", "", ucac)
