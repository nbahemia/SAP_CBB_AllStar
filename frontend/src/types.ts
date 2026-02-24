// src/types.ts

export interface Filters {
  teams: string[]
  years: number[]
  positions: string[]
}

export interface Player {
  pid: number
  player_name: string
  team: string
  year: number
  simple_pos: string
}

export interface PredictResult {
  player_name: string
  team: string
  position: string
  year: number
  all_star_probability: number
  raw_stats: Record<string, number>
  percentile_stats: Record<string, number>
}