import joblib
import os

BASE_DIR = os.path.dirname(os.path.abspath(__file__))

models = {}  

def load_model(position: str):
    position = position.upper()
    if position not in ["G", "F", "C"]:
        raise ValueError(f"Invalid position '{position}'. Must be G, F, or C.")

    if position not in models:
        path = os.path.join(BASE_DIR, "models", f"{position.lower()}_model.pkl")
        if not os.path.exists(path):
            raise FileNotFoundError(f"Model for {position} not found at {path}")
        models[position] = joblib.load(path)
    return models[position]