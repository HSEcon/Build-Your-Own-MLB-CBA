"""Build an independently calculated, redistributable Retrosheet-only player dataset.
No proprietary WAR, DRS, OAA, MLB API, or previous private player inputs.
2015-2025 observations, two-year-ahead 2027 scenario; no invented 2026 rows.
"""
import csv, io, json, math, hashlib, urllib.request, zipfile
from collections import defaultdict, Counter
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
BASE="https://www.retrosheet.org/downloads/"
NOTICE='The information used here was obtained free of charge from and is copyrighted by Retrosheet. Interested parties may contact Retrosheet at "www.retrosheet.org".'
OUT=Path("data/hwar_preview")
CACHE=Path("/tmp/retrosheet-release")
CACHE.mkdir(exist_ok=True)
OUT.mkdir(parents=True,exist_ok=True)
MAP={"CHA":"CHW","KCA":"KCR","LAN":"LAD","NYA":"NYY","NYN":"NYM","SDN":"SDP","SFN":"SFG","SLN":"STL","TBA":"TBR","WSN":"WSH","WAS":"WSH","OAK":"ATH","ANA":"LAA","FLO":"MIA","CHN":"CHC"}
POS={2:"C",3:"1B",4:"2B",5:"3B",6:"SS",7:"LF",8:"CF",9:"RF",10:"DH"}
ADJ={2:12.5,3:-12.5,4:2.5,5:2.5,6:7.5,7:-7.5,8:2.5,9:-7.5,10:-17.5}
B="b_pa b_ab b_h b_d b_t b_hr b_r b_rbi b_w b_k b_hbp b_sb b_cs b_sf b_sh b_iw b_gdp".split()
P="p_ipouts p_bfp p_h p_hr p_r p_er p_w p_k p_hbp p_wp p_bk".split()
F="d_ifouts d_po d_a d_e d_dp d_pb d_sb d_cs".split()
def fetch(url):
    dest=CACHE/url.rsplit("/",1)[-1]
    if not dest.exists():
        with urllib.request.urlopen(url,timeout=180) as response: data=response.read()
        dest.write_bytes(data)
    raw=dest.read_bytes()
    return zipfile.ZipFile(io.BytesIO(raw)),hashlib.sha256(raw).hexdigest()
def read(z,name):
    matches=[n for n in z.namelist() if n.rsplit("/",1)[-1].lower().endswith(name.lower()) or n.rsplit("/",1)[-1].lower().startswith(name.lower().removesuffix(".csv")) and n.lower().endswith(".csv")]
    if len(matches)!=1: raise ValueError((name,z.namelist(),matches))
    n=matches[0]
    with z.open(n) as f: yield from csv.DictReader(io.TextIOWrapper(f,encoding="utf-8-sig",errors="strict"))
def num(r,k):
    v=r[k]
    if v in ("",None): return 0
    return int(float(v))
def team(x):return MAP.get(x,x)
def div(a,b):return a/b if b else None
def add(d,r,cols):
    for k in cols:d[k]+=num(r,k)
def season(y):
    url=BASE+f"{y}/{y}csvs.zip"
    z,digest=fetch(url)
    games={r["gid"] for r in read(z,"gameinfo.csv") if r["gametype"].lower() in ("regular","regular season","regular-season","reg")}
    assert len(games) in range(850,2501),(y,len(games))
    if y!=2020:assert len(games)>=2400,(y,len(games))
    totals=defaultdict(Counter); teams=defaultdict(Counter); positions=defaultdict(Counter); fld=defaultdict(Counter)
    names={r["id"]:r["first"]+" "+r["last"] for r in read(z,"allplayers.csv")}
    for name,cols in [("batting.csv",B),("pitching.csv",P),("fielding.csv",F)]:
        seen=set()
        for r in read(z,name):
            if r["gid"] not in games or r.get("stattype","value")!="value":continue
            pid=r["id"];t=team(r["team"])
            key=(r["gid"],pid,t,r.get("d_pos",""),r.get("d_seq",""))
            assert key not in seen,(y,name,key)
            seen.add(key)
            if name=="fielding.csv":
                pos=int(r["d_pos"]);add(fld[(pid,pos)],r,cols)
                positions[pid][pos]+=num(r,"d_ifouts")
            else:
                add(totals[pid],r,cols)
                totals[pid]["batting_games" if name=="batting.csv" else "pitching_games"]+=1
                if name=="pitching.csv":totals[pid]["starts"]+=int(r.get("gs","") in ("1","Y","y"))
                teams[pid][t]+=num(r,"b_pa" if name=="batting.csv" else "p_ipouts")
        assert seen,(y,name)
    allc=Counter()
    for t in totals.values():allc.update(t)
    assert 0<allc["b_h"]<allc["b_ab"]<allc["b_pa"] and allc["p_ipouts"]>0
    assert abs(allc["b_r"]-allc["p_r"])<=5,(y,allc["b_r"],allc["p_r"])
    lg=defaultdict(Counter)
    for (pid,pos),f in fld.items():lg[pos].update(f)
    def bat(c):return .69*(c["b_w"]-c["b_iw"])+.72*c["b_hbp"]+.88*(c["b_h"]-c["b_d"]-c["b_t"]-c["b_hr"])+1.25*c["b_d"]+1.58*c["b_t"]+2.03*c["b_hr"]
    bavg=bat(allc)/allc["b_pa"]
    ra=27*allc["p_r"]/allc["p_ipouts"]
    era=27*allc["p_er"]/allc["p_ipouts"]
    fipraw=lambda c:13*c["p_hr"]+3*(c["p_w"]+c["p_hbp"])-2*c["p_k"]
    fipc=era-3*fipraw(allc)/allc["p_ipouts"]
    # Explicit illustrative runs/win scale tied to season scoring.
    rpw=1.5*ra+3
    out=[]
    for pid,c in totals.items():
        pa=c["b_pa"];ip=c["p_ipouts"]/3
        pos=positions[pid];field=0;position=0;outs=0;chances=0;errors=0
        for p,oc in pos.items():
            if p not in ADJ:continue
            f=fld[(pid,p)];g=lg[p]
            ch=f["d_po"]+f["d_a"]+f["d_e"];gch=g["d_po"]+g["d_a"]+g["d_e"]
            # Error avoidance on recorded chances only; no claim to measure range.
            if gch and p!=10:field+=.7*(ch*g["d_e"]/gch-f["d_e"]);chances+=ch;errors+=f["d_e"]
            position+=ADJ[p]*oc/4050;outs+=oc
        if pa and not outs and not ip:position=-17.5*pa/600
        br=bat(c)-pa*bavg;run=.2*c["b_sb"]-.4*c["b_cs"]
        # No park correction; pitching uses FIP only to avoid treating team fielding as individual pitcher skill.
        fip=fipraw(c)/ip+fipc if ip else None
        pr=(era-fip)*ip/9 if ip else 0
        brep=20*pa/600;prep=ip/9
        hit=(br+run+field+position+brep)/rpw;pitch=(pr+prep)/rpw
        ab=c["b_ab"];avg=div(c["b_h"],ab);obp=div(c["b_h"]+c["b_w"]+c["b_hbp"],ab+c["b_w"]+c["b_hbp"]+c["b_sf"]);slg=div(c["b_h"]+c["b_d"]+2*c["b_t"]+3*c["b_hr"],ab)
        pmax=max(((oc,p) for p,oc in pos.items() if p in POS),default=(0,10))[1]
        d=dict(c);d.update(Season=y,ID=pid,Player=names.get(pid,pid),Teams=";".join(sorted(teams[pid])),
          Position="TWP" if ip>=20 and pa>=100 else ("P" if ip and not pa>=100 else POS[pmax]),PA=pa,IP=ip,
          hWAR=hit+pitch,Hitting_hWAR=hit,Pitching_hWAR=pitch,AVG=avg,OBP=obp,SLG=slg,OPS=obp+slg if obp is not None and slg is not None else None,
          ERA=div(9*c["p_er"],ip),RA9=div(9*c["p_r"],ip),WHIP=div(c["p_h"]+c["p_w"],ip),FIP=fip,
          Batting_Runs=br,Running_Runs=run,Defense_Runs=field if chances else None,Position_Runs=position,Pitch_Runs=pr,FIP_Runs=pr,
          RA9_Runs=None,Bat_Replacement=brep,Pitch_Replacement=prep,Runs_Per_Win=rpw,Recorded_Fielding_Chances=chances,Recorded_Fielding_Errors=errors,
          Defense_Coverage="Recorded-error proxy only; no range" if chances else "No measured individual fielding",Park_Coverage="No park adjustment",
          Status="Retrosheet regular-season estimate",Age=None,Healthy_hWAR=None,Healthy_PA=None,Healthy_IP=None)
        out.append(d)
    assert len(out)>1100 and len({d["ID"] for d in out})==len(out),(y,len(out))
    print(y,len(out),len(games),allc["b_pa"],flush=True)
    return out,dict(year=y,url=url,sha256=digest,games=len(games),players=len(out),league_pa=allc["b_pa"])
def project(rows,origin,target):
    h=defaultdict(list)
    for x in rows:
        if origin-2<=x["Season"]<=origin:h[x["ID"]].append(x)
    out=[]
    for pid,xx in h.items():
        a=next((x for x in xx if x["Season"]==origin),None)
        if a is None:continue
        def part(work,war):
            weights=[(3-(origin-x["Season"]),x) for x in xx]
            # shrink per-opportunity production toward replacement, and workload to a recent weighted mean.
            total=sum(w*x[work] for w,x in weights)
            rate=sum(w*x[war] for w,x in weights)/(total+(150 if work=="PA" else 40))
            amount=total/sum(w for w,x in weights)
            healthy=max(x[work] for x in xx)
            return amount,healthy,amount*rate,healthy*rate
        pa,hpa,hit,hh=part("PA","Hitting_hWAR");ip,hip,pit,hp=part("IP","Pitching_hWAR")
        d={k:None for k in a}
        d.update(Season=target,ID=pid,Player=a["Player"],Teams=a["Teams"],Position=a["Position"],
          Age=a["Age"]+target-origin if a["Age"] is not None else None,PA=pa,IP=ip,Healthy_PA=hpa,Healthy_IP=hip,
          hWAR=hit+pit,Hitting_hWAR=hit,Pitching_hWAR=pit,Healthy_hWAR=hh+hp,Status=f"{target-origin}-year scenario from {origin}; no 2026 observations" if target==2027 else "Retrospective scenario",
          Defense_Coverage="Embedded recorded-error proxy; no separate projection",Park_Coverage="No park adjustment")
        out.append(d)
    return out
def write_csv(path,rows):
    cols=list(dict.fromkeys(k for x in rows for k in x))
    with path.open("w",encoding="utf-8",newline="") as f:
        w=csv.DictWriter(f,fieldnames=cols);w.writeheader();w.writerows(rows)
def main():
    with ThreadPoolExecutor(max_workers=3) as pool:results=list(pool.map(season,range(2015,2026)))
    rows=[x for a,_ in results for x in a]
    z,sha=fetch(BASE+"biodata.zip")
    bio={x["id"]:x for x in read(z,"biofile0.csv")}
    for x in rows:
        b=bio.get(x["ID"],{});dob=b.get("birthdate","")
        if len(dob)==8 and dob.isdigit():x["Age"]=x["Season"]-int(dob[:4])-(dob[4:]>"0701")
    assert all(x["Player"]!=x["ID"] and x["Age"] is not None for x in rows)
    assert all(set(x["Teams"].split(";")) <= set("ARI ATL BAL BOS CHC CHW CIN CLE COL DET HOU KCR LAA LAD MIA MIL MIN NYM NYY ATH PHI PIT SDP SEA SFG STL TBR TEX TOR WSH".split()) for x in rows)
    audits=[]
    for year in (2023,2024,2025):
        truth={x["ID"]:x["hWAR"] for x in rows if x["Season"]==year}
        base={x["ID"]:x["hWAR"] for x in rows if x["Season"]==year-2}
        pred=project(rows,year-2,year);matched=[x for x in pred if x["ID"] in truth]
        rmse=lambda e:math.sqrt(sum(v*v for v in e)/len(e))
        audits.append(dict(target=year,n=len(matched),model_rmse=rmse([x["hWAR"]-truth[x["ID"]] for x in matched]),baseline_rmse=rmse([base[x["ID"]]-truth[x["ID"]] for x in matched])))
    forecasts=project(rows,2025,2027);rows+=forecasts
    for year in sorted({x["Season"] for x in rows}):
        for rank,x in enumerate(sorted((x for x in rows if x["Season"]==year),key=lambda x:-x["hWAR"]),1):x["Rank"]=rank
    assert len({(x["Season"],x["ID"]) for x in rows})==len(rows)
    assert all(abs(x["hWAR"]-x["Hitting_hWAR"]-x["Pitching_hWAR"])<1e-8 for x in rows)
    write_csv(OUT/"players.csv",rows)
    (OUT/"open_data_audit.json").write_text(json.dumps(dict(notice=NOTICE,source_license="https://www.retrosheet.org/notice.txt",source_seasons=[m for _,m in results],biography_sha256=sha,history_rows=len(rows)-len(forecasts),forecast_rows=len(forecasts),two_year_holdouts=audits),indent=2))
    (OUT/"NOTICE.txt").write_text(NOTICE+"\n")
    print(json.dumps(audits),len(rows),len(forecasts))
if __name__=="__main__":main()
