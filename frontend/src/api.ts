import type { Filters, Player, PredictResult } from "./types"

const BASE = "http://localhost:8000"

export async function getFilters(): Promise<Filters> {
  const res = await fetch(`${BASE}/filters`)
  if (!res.ok) throw new Error("Failed to load filters")
  return res.json()
}

export async function getPlayers({
  position,
  team,
  year,
}: {
  position?: string
  team?: string
  year?: string
}): Promise<Player[]> {
  const params = new URLSearchParams()
  if (position) params.set("position", position)
  if (team) params.set("team", team)
  if (year) params.set("year", year)
  const res = await fetch(`${BASE}/players?${params}`)
  if (!res.ok) throw new Error("Failed to load players")
  return res.json()
}

export async function predict(pid: number, year: number): Promise<PredictResult> {
  const res = await fetch(`${BASE}/predict`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ pid, year }),
  })
  if (!res.ok) throw new Error((await res.json()).detail)
  return res.json()
}