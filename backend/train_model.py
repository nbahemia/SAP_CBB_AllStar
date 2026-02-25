import os
import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
import joblib

# Paths
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
data_path = os.path.join(BASE_DIR, "data", "CBB_labeled.csv")
models_dir = os.path.join(BASE_DIR, "models")

# Create models folder if it doesn't exist
os.makedirs(models_dir, exist_ok=True)

# Load dataset
df = pd.read_csv(data_path)

# Filter training years
df = df[df["year"] <= 2021]

# ── Fix stat directions ───────────────────────────────────────────────────────
# These stats are lower = better, so invert them so the model
# correctly learns that lower values are positive signals

# Rename for easier handling
df = df.rename(columns={"ast/tov": "ast_tov"})

# Fix stat directions
df["drtg"]   = -df["drtg"]
df["adrtg"]  = -df["adrtg"]
df["TO_per"] = -df["TO_per"]

# ─────────────────────────────────────────────────────────────────────────────

features = [
    "height_in", "Ortg", "usg", "eFG", "TS_per",
    "ORB_per", "DRB_per", "AST_per", "TO_per",
    "blk_per", "stl_per", "porpag", "adjoe",
    "drtg", "adrtg", "dporpag",
    "bpm", "ast_tov"
]


# Positions to train
positions = ["G", "F", "C"]

for pos in positions:
    print(f"Training model for {pos}s...")

    # Filter by position
    pos_df = df[df["simple_pos"] == pos].copy()

    # Latest season per player
    pos_df = pos_df.sort_values("year").groupby("player_name").tail(1)

    X = pos_df[features]
    y = pos_df["is_all_star"]

    # Drop rows where any feature is NaN
    mask = X.notna().all(axis=1)
    X = X[mask]
    y = y[mask]

    print(f"  Sample size: {len(y)} | All-Stars: {int(y.sum())} | Non All-Stars: {int((y == 0).sum())}")

    # Build pipeline — class_weight="balanced" handles the heavy class imbalance
    # (~49 All-Stars vs thousands of non-All-Stars)
    pipeline = Pipeline([
        ("scaler", StandardScaler()),
        ("model", LogisticRegression(
            penalty="l2",
            solver="liblinear",
            class_weight={0: 1, 1: 20}  # tune this number down from ~50 that balanced uses
        ))
    ])

    # Hyperparameter tuning
    param_grid = {"model__C": np.logspace(-4, 1, 20)}
    grid = GridSearchCV(pipeline, param_grid, cv=5, scoring="roc_auc")

    # Fit model
    grid.fit(X, y)
    print(f"  Best AUC: {grid.best_score_:.4f}")

    # Save model
    model_path = os.path.join(models_dir, f"{pos.lower()}_model.pkl")
    joblib.dump(grid.best_estimator_, model_path)
    print(f"  Saved to {model_path}\n")