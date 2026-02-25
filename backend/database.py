import sqlite3
import pandas as pd
import os

# Build path relative to this file's location
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
CSV_PATH = os.path.join(BASE_DIR, "data", "CBB_labeled.csv")
DB_PATH = os.path.join(BASE_DIR, "cbb.db")

def init_db():
    """Load CSV into SQLite if not already done."""
    if os.path.exists(DB_PATH):
        return
    df = pd.read_csv(CSV_PATH)
    df = df.rename(columns={"ast/tov": "ast_tov"})  # ← add this
    conn = sqlite3.connect(DB_PATH)
    df.to_sql("players", conn, if_exists="replace", index=False)
    conn.close()
    print("Database initialized.")

def get_connection():
    return sqlite3.connect(DB_PATH)

def get_filters():
    """Return distinct values for filter dropdowns."""
    conn = get_connection()
    teams = pd.read_sql("SELECT DISTINCT team FROM players WHERE team IS NOT NULL ORDER BY team", conn)["team"].tolist()
    years = pd.read_sql("SELECT DISTINCT year FROM players WHERE year IS NOT NULL ORDER BY year DESC", conn)["year"].tolist()
    positions = ["G", "F", "C"]
    conn.close()
    return {"teams": teams, "years": years, "positions": positions}

def get_players(position=None, team=None, year=None):
    """Return list of players matching filters."""
    conn = get_connection()
    query = "SELECT DISTINCT pid, player_name, team, year, simple_pos FROM players WHERE 1=1"
    params = []
    if position:
        query += " AND simple_pos = ?"
        params.append(position)
    if team:
        query += " AND team = ?"
        params.append(team)
    if year:
        query += " AND year = ?"
        params.append(int(year))
    query += " ORDER BY player_name"
    df = pd.read_sql(query, conn, params=params)
    conn.close()
    return df.to_dict(orient="records")

def get_player_stats(pid, year):
    """Return full stats row for a player."""
    conn = get_connection()
    df = pd.read_sql(
        "SELECT * FROM players WHERE pid = ? AND year = ? LIMIT 1",
        conn, params=[pid, int(year)]
    )
    conn.close()
    if df.empty:
        return None
    return df.iloc[0].to_dict()