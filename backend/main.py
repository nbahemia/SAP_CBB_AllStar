from fastapi import FastAPI, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import Optional
import pandas as pd
from utils import load_model
from database import init_db, get_filters, get_players, get_player_stats

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
    "drtg", "adrtg", "dporpag",
    "bpm", "ast_tov"
]

PERCENTILE_FEATURES = [
    "Ortg_percentile", "usg_percentile", "eFG_percentile", "TS_per_percentile",
    "ORB_per_percentile", "DRB_per_percentile", "AST_per_percentile", "TO_per_percentile",
    "blk_per_percentile", "stl_per_percentile", "porpag_percentile", "adjoe_percentile",
    "drtg_percentile", "adrtg_percentile", "dporpag_percentile"
]

# ── Filter options ─────────────────────────────────────────────────────────────

@app.get("/filters")
def filters():
    return get_filters()

# ── Player list ────────────────────────────────────────────────────────────────

@app.get("/players")
def players(
    position: Optional[str] = Query(None),
    team: Optional[str] = Query(None),
    year: Optional[int] = Query(None),
):
    return get_players(position=position, team=team, year=year)

# ── Predict by pid + year ──────────────────────────────────────────────────────

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

    # Invert lower-is-better stats to match training
    row["drtg"]   = -row["drtg"]
    row["adrtg"]  = -row["adrtg"]
    row["TO_per"] = -row["TO_per"]

    # Handle ast/tov column rename
    if "ast/tov" in row and "ast_tov" not in row:
        row["ast_tov"] = row["ast/tov"]

    try:
        model = load_model(pos)
        features = pd.DataFrame([row])[FEATURES]
        prob = float(model.predict_proba(features)[:, 1][0])
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

    # Raw stats for the chart (exclude height, include bpm and ast_tov)
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

# ── Manual predict ─────────────────────────────────────────────────────────────

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
    bpm: float
    ast_tov: float

@app.post("/predict/manual")
def predict_manual(player: PlayerInput):
    try:
        data = player.dict()
        data["drtg"]   = -data["drtg"]
        data["adrtg"]  = -data["adrtg"]
        data["TO_per"] = -data["TO_per"]
        model = load_model(data["simple_pos"])
        features = pd.DataFrame([data])[FEATURES]
        prob = float(model.predict_proba(features)[:, 1][0])
        return {"all_star_probability": round(prob * 100, 1)}
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

# ── Model feature importance ───────────────────────────────────────────────────

@app.get("/model-info/{position}")
def model_info(position: str):
    position = position.upper()
    if position not in ["G", "F", "C"]:
        raise HTTPException(status_code=400, detail="Invalid position")
    model = load_model(position)
    lr = model.named_steps["model"]
    features = [
        "height_in", "Ortg", "usg", "eFG", "TS_per",
        "ORB_per", "DRB_per", "AST_per", "TO_per",
        "blk_per", "stl_per", "porpag", "adjoe",
        "drtg", "adrtg", "dporpag",
        "bpm", "ast_tov"
    ]
    coefs = list(zip(features, lr.coef_[0].tolist()))
    top = sorted(coefs, key=lambda x: abs(x[1]), reverse=True)[:5]
    return {"position": position, "top_features": [f for f, _ in top]}