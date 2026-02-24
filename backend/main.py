from fastapi import FastAPI, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import Optional
import pandas as pd
from backend.utils import load_model
from backend.database import init_db, get_filters, get_players, get_player_stats

app = FastAPI(title="CBB Player Prediction API")

# Allow React dev server to call the API
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:5173", "http://localhost:3000"],
    allow_methods=["*"],
    allow_headers=["*"],
)

# Initialize DB on startup
@app.on_event("startup")
def startup():
    init_db()

FEATURES = [
    "height_in", "Ortg", "usg", "eFG", "TS_per",
    "ORB_per", "DRB_per", "AST_per", "TO_per",
    "blk_per", "stl_per", "porpag", "adjoe",
    "drtg", "adrtg", "dporpag"
]

PERCENTILE_FEATURES = [
    "Ortg_percentile", "usg_percentile", "eFG_percentile", "TS_per_percentile",
    "ORB_per_percentile", "DRB_per_percentile", "AST_per_percentile", "TO_per_percentile",
    "blk_per_percentile", "stl_per_percentile", "porpag_percentile", "adjoe_percentile",
    "drtg_percentile", "adrtg_percentile", "dporpag_percentile"
]

# ── Filter options ────────────────────────────────────────────────────────────

@app.get("/filters")
def filters():
    return get_filters()

# ── Player list ───────────────────────────────────────────────────────────────

@app.get("/players")
def players(
    position: Optional[str] = Query(None),
    team: Optional[str] = Query(None),
    year: Optional[int] = Query(None),
):
    return get_players(position=position, team=team, year=year)

# ── Predict by pid + year ─────────────────────────────────────────────────────

class PredictRequest(BaseModel):
    pid: int
    year: int

@app.post("/predict")
def predict(req: PredictRequest):
    row = get_player_stats(req.pid, req.year)
    if row is None:
        raise HTTPException(status_code=404, detail="Player not found")

    pos = row.get("simple_pos")
    if pos not in ["G", "F", "C"]:
        raise HTTPException(status_code=400, detail=f"Invalid position: {pos}")

    try:
        model = load_model(pos)
        features = pd.DataFrame([row])[FEATURES]
        prob = float(model.predict_proba(features)[:, 1][0])
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

    # Raw stats for the chart
    raw_stats = {f: row.get(f) for f in FEATURES if f != "height_in"}

    # Percentile stats for the chart
    percentile_stats = {f: row.get(f) for f in PERCENTILE_FEATURES}

    return {
        "player_name": row.get("player_name"),
        "team": row.get("team"),
        "position": pos,
        "year": req.year,
        "all_star_probability": round(prob * 100, 1),
        "raw_stats": raw_stats,
        "percentile_stats": percentile_stats,
    }

# ── Manual predict (keep original for testing) ────────────────────────────────

class PlayerInput(BaseModel):
    simple_pos: str
    height_in: float
    Ortg: float
    usg: float
    eFG: float
    TS_per: float
    ORB_per: float
    DRB_per: float
    AST_per: float
    TO_per: float
    blk_per: float
    stl_per: float
    porpag: float
    adjoe: float
    drtg: float
    adrtg: float
    dporpag: float

@app.post("/predict/manual")
def predict_manual(player: PlayerInput):
    try:
        model = load_model(player.simple_pos)
        features = pd.DataFrame([player.dict()])[FEATURES]
        prob = float(model.predict_proba(features)[:, 1][0])
        return {"all_star_probability": round(prob * 100, 1)}
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))