"""Actual-data browser validation for the open player model, with native or Shinylive runtime."""
import csv
from pathlib import Path
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import StaleElementReferenceException,ElementClickInterceptedException
o=webdriver.ChromeOptions()
for a in ("--headless=new","--no-sandbox","--disable-dev-shm-usage","--window-size=1440,1000"):o.add_argument(a)
d=webdriver.Chrome(options=o)
w=WebDriverWait(d,90,ignored_exceptions=(StaleElementReferenceException,ElementClickInterceptedException))
out=Path("open-release-evidence");out.mkdir(exist_ok=True)
def visible(s):
 return w.until(lambda d:next((e for e in d.find_elements(By.CSS_SELECTOR,s) if e.is_displayed()),False))
def text(s,t):
 return w.until(lambda d:next((e for e in d.find_elements(By.CSS_SELECTOR,s) if e.is_displayed() and t in e.text),False))
def click(e):
 d.execute_script("arguments[0].scrollIntoView({block:'center'})",e);e.click()
def radio(g,n):click(text("#"+g+" label",n))
def locate(depth=0):
 if d.find_elements(By.CSS_SELECTOR,"#payroll_plot .plotly"):return True
 if depth>=3:return False
 for f in d.find_elements(By.CSS_SELECTOR,"iframe"):
  try:
   d.switch_to.frame(f)
   if locate(depth+1):return True
   d.switch_to.parent_frame()
  except StaleElementReferenceException:
   d.switch_to.default_content();return False
 return False
def ready(_):
 d.switch_to.default_content();return locate()
try:
 d.get("http://127.0.0.1:3838/")
 WebDriverWait(d,300).until(ready)
 radio("nav_group","Player Analytics");radio("player_tab","hWAR Leaders")
 visible("#fo_search").send_keys("Aaron Judge")
 click(text("#fo_leader_table a.fo-player-link","Aaron Judge"))
 text("#fo_profile_identity","Aaron Judge");text("#fo_history thead","Healthy hWAR");visible("#fo_history tbody td")
 text("#fo_component_table","Total projected hWAR")
 text("#fo_standard_bat_panel","Batting");text("#fo_standard_bat","Dataset total");text("#fo_standard_bat thead","OPS")
 visible("#fo_standard_summary .fo-kpi")
 click(text(".fo-shortcuts a","Back to player rankings"))
 visible("#fo_leader_table tbody td")
 from selenium.webdriver.support.ui import Select
 Select(visible("#fo_year")).select_by_value("2027")
 w.until(lambda d: "Healthy hWAR" in [e.get_attribute("value") for e in Select(d.find_element(By.ID,"fo_sort")).options])
 assert "OPS" not in [e.get_attribute("value") for e in Select(d.find_element(By.ID,"fo_sort")).options]
 Select(visible("#fo_year")).select_by_value("2025")
 w.until(lambda d:"OPS" in [e.get_attribute("value") for e in Select(d.find_element(By.ID,"fo_sort")).options])
 assert "Healthy hWAR" not in [e.get_attribute("value") for e in Select(d.find_element(By.ID,"fo_sort")).options]
 click(text("#fo_leader_table a.fo-player-link","Aaron Judge"))
 text("#fo_profile_identity","Aaron Judge")
 print("PASS standard profile, rankings navigation and season-specific sorting",flush=True)
 d.save_screenshot(str(out/"01-profile.png"))
 radio("nav_group","Front Office");radio("fo_tab","Projected Standings")
 text("#fo_division_standings","AL East");text("#fo_division_standings","NL West")
 w.until(lambda d:len(d.find_elements(By.CSS_SELECTOR,"#fo_division_standings a.fo-team-link"))==30)
 click(visible("#fo_division_standings a[data-fo-team=\"BAL\"]"))
 text("#team_profile_identity","Baltimore Orioles");visible("#team_profile_wins .plotly")
 click(text("#team_profile_tabs a","Roster & projections"));text("#team_profile_roster_note","Not a verified current roster");visible("#team_profile_roster a.fo-player-link")
 click(visible("#team_profile_back"));visible("#fo_division_standings")
 radio("nav_group","MLB Financial Data");radio("finance_tab","Live Payroll")
 click(text("#live_payroll_table a.fo-team-link","Baltimore Orioles"));text("#team_profile_identity","Baltimore Orioles")
 click(visible("#team_profile_back"));visible("#live_payroll_table")
 print("PASS division standings, club links, team roster and navigation",flush=True)
 radio("nav_group","MLB Financial Data");radio("finance_tab","Transactions")
 click(text("#transaction_view a","Trades"))
 row=text("#trade_history_table tbody tr","Christian Koss");click(row.find_element(By.TAG_NAME,"td"))
 visible("#shiny-modal #trade_detail");click(visible("#analyze_selected_tx"))
 visible("#fo_actual_chart .plotly");visible("#fo_actual_clubs tbody td")
 assert not d.find_elements(By.ID,"tx_wins_plot")
 visible("#fo_actual_asset_future_wins .plotly");visible("#fo_actual_asset_cumulative_wins .plotly");visible("#fo_actual_asset_career_paths .plotly");text("#fo_actual_asset_clubs","Incomplete; no net valuation")
 d.save_screenshot(str(out/"02-trade.png"))
 print("PASS actual Retrosheet player profile and reported trade / future wins.")
except Exception:
 d.save_screenshot(str(out/"failure.png"));(out/"failure.html").write_text(d.page_source)
 raise
finally:d.quit()
