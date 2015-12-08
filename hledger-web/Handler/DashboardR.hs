{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
-- | /dashboard handlers.

module Handler.DashboardR where

import Import
import Data.Time.Calendar
import Safe

import Handler.AddForm
import Handler.Common
import Handler.Utils

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
    ^{postingsMonthlyHistory opts today am j}
  |]
  where postingsMonthlyHistory opts today am j =
          postingsMonthlyHistoryChartAsHtml today $ postingsReport (reportopts_ $ cliopts_ opts){depth_=Just 1, monthly_=True} postingsMonthlyHistoryQuery j

postingsMonthlyHistoryChartAsHtml :: Day -> PostingsReport -> HtmlUrl AppRoute
postingsMonthlyHistoryChartAsHtml today (totallabel, items') =
  [hamlet|
<label#postings-chart-label style=""><br>
<div#postings-chart style="width:85%; height:150px; margin-bottom:1em; display:block;">
<script type=text/javascript>
 \$(document).ready(function() {
   var $chartdiv = $('#postings-chart');
   if ($chartdiv.is(':visible')) {
     \$('#postings-chart-label').text('expenses');
     var seriesData = [
       {
        data: [
          $forall i <- items'
           [
            #{dayToJsTimestamp $ postingReportItemDay i},
            #{simpleMixedAmountQuantity $ pamount $ posting i}
           ],
        ],
        label: 'expenses',
        lines: {
          show: true,
          steps: true,
        },
        points: {
          show: false,
        },
        clickable: false,
        hoverable: false,
       },
     ]
     var plot = registerChart($chartdiv, seriesData);
     \$chartdiv.bind("plotclick", registerChartClick);
   };
 });
  |]
  where
    simpleMixedAmountQuantity = maybe 0 aquantity . headMay . amounts
    posting (_, _, _, p, _) = p
    postingReportItemDay (Just d,  _, _, _, _) = d
    postingReportItemDay (Nothing, _, _, _, _) = today
