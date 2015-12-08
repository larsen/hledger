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

postingsMonthlyHistoryQuery :: Query
postingsMonthlyHistoryQuery = And ([Depth 1,Acct "expenses"])

getDashboardR :: Handler Html
getDashboardR = do
  vd@VD{..} <- getViewData
  hledgerLayout vd "dashboard" [hamlet|
    <h2#contenttile>Dashboard
    ^{postingsMonthlyHistory opts am j}
  |]
  where postingsMonthlyHistory opts am j =
          postingsMonthlyHistoryChartAsHtml $ postingsReport (reportopts_ $ cliopts_ opts){depth_=Just 1, monthly_=True} postingsMonthlyHistoryQuery j

postingsMonthlyHistoryChartAsHtml :: PostingsReport -> HtmlUrl AppRoute
postingsMonthlyHistoryChartAsHtml (totallabel, items') =
  [hamlet|
    <p>
      $forall i <- items'
        ^{itemAsHtml i}
  |]
  where
    itemAsHtml (Just day, _, description, posting, amount) =
      [hamlet|#{show day} #{mixedAmountAsHtml $ pamount posting}<br>|]
    itemAsHtml (Nothing,  _, description, _, amount) =
      [hamlet|<br>|]
