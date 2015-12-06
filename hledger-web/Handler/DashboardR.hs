{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
-- | /dashboard handlers.

module Handler.DashboardR where

import Import

import Handler.AddForm
import Handler.Common

import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Utils
import Hledger.Cli.CliOptions
import Hledger.Web.WebOptions

getDashboardR :: Handler Html
getDashboardR = do
  vd@VD{..} <- getViewData
  hledgerLayout vd "dashboard" [hamlet|
    <h2#contenttile>Dashboard
  |]
