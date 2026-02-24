import joblib
import os

models = {}

def load_model(position: str):
    """
    Load a model for a given position: G, F, C
    Caches models in memory so they are loaded only once.
    """
    position = position.upper()
    if position not in ["G", "F", "C"]:
        raise ValueError(f"Invalid position '{position}'. Must be G, F, or C.")

    if position not in models:
        path = os.path.join("models", f"{position.lower()}_model.pkl")
        if not os.path.exists(path):
            raise FileNotFoundError(f"Model for {position} not found at {path}")
        models[position] = joblib.load(path)
    return models[position]