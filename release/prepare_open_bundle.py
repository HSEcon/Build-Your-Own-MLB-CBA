"""Create a release candidate using only the existing public facts and licensed player data."""
import json,hashlib,csv,subprocess,sys
from pathlib import Path
root=Path.cwd()
public=Path(sys.argv[1])
out=Path(sys.argv[2])
existing=json.loads((public/"site/app.json").read_text())
code=["app.R","trade_ledger.R","scenario_accounting.R","bargaining_model.R","optimizer.R","research_validation.R","transaction_impact.R","transaction_ui.R","front_office_preview.R","matchup_preview.R","player_financial_bridge.R"]
player=["data/hwar_preview/players.csv","data/hwar_preview/open_data_audit.json","data/hwar_preview/NOTICE.txt"]
allowed=[x for x in existing if x["name"] not in code]
assert all(x["name"].startswith("data/") or x["name"] in ("mlb_model1_project_data_2011_2025.csv","mlb_model2_project_data_2017_2025.csv") for x in allowed)
assert all("pickaleague" not in x["content"].lower() for x in allowed if x["name"].startswith("data/"))
files=allowed+[{"name":n,"type":"text","content":(root/n).read_text()} for n in code+player]
assert len({x["name"] for x in files})==len(files)
audit=json.loads((root/player[1]).read_text())
with (root/player[0]).open() as f:rows=list(csv.DictReader(f))
assert len(rows)==audit["history_rows"]+audit["forecast_rows"]
assert not any(x["Season"]=="2026" for x in rows)
assert all(x["Age"] and x["ID"] and x["Player"] for x in rows)
out.mkdir(parents=True,exist_ok=True)
payload=json.dumps(files,ensure_ascii=False,separators=(",",":")).encode()
(out/"app.json").write_bytes(payload)
(out/"manifest.json").write_text(json.dumps({"source_commit":subprocess.check_output(["git","rev-parse","HEAD"],text=True).strip(),"public_baseline_commit":subprocess.check_output(["git","-C",str(public),"rev-parse","HEAD"],text=True).strip(),"sha256":hashlib.sha256(payload).hexdigest(),"player_audit":audit,"files":[{"name":x["name"],"sha256":hashlib.sha256(x["content"].encode()).hexdigest()} for x in files]},indent=2))
print("Candidate",len(payload),"bytes",len(rows),"player records")
