import { useState } from "react"
import {
  RadarChart,
  Radar,
  PolarGrid,
  PolarAngleAxis,
  PolarRadiusAxis,
  ResponsiveContainer,
  Tooltip,
} from "recharts"
import type { PredictResult } from "../types"
import { RAW_LABELS, PERCENTILE_LABELS } from "../constants"

interface StatsRadarProps {
  result: PredictResult
}

type ChartMode = "percentile" | "raw"

export default function StatsRadar({ result }: StatsRadarProps) {
  const [mode, setMode] = useState<ChartMode>("percentile")

  const chartData =
    mode === "percentile"
      ? Object.entries(PERCENTILE_LABELS).map(([key, label]) => ({
          stat: label,
          value:
            result.percentile_stats?.[key] != null
              ? Math.round(result.percentile_stats[key] * 100) / 100
              : 0,
          fullMark: 100,
        }))
      : Object.entries(RAW_LABELS).map(([key, label]) => ({
          stat: label,
          value:
            result.raw_stats?.[key] != null
              ? Math.round(result.raw_stats[key] * 100) / 100
              : 0,
          fullMark: undefined,
        }))

  return (
    <div className="bg-[#0f1e30] border border-[#1e3a5f] rounded-2xl p-6">
      {/* Header */}
      <div className="flex justify-between items-center mb-5">
        <span className="text-xs text-[#7a8fa6] tracking-[3px] uppercase">Player Profile</span>
        <div className="flex gap-2">
          <button
            onClick={() => setMode("percentile")}
            className={`px-4 py-1.5 rounded-md text-xs border transition-all ${
              mode === "percentile"
                ? "bg-[#1e3a5f] text-[#e8f0fe] border-[#2d7dd2]"
                : "bg-transparent text-[#7a8fa6] border-[#1e3a5f] hover:border-[#2d7dd2]"
            }`}
          >
            Percentile
          </button>
          <button
            onClick={() => setMode("raw")}
            className={`px-4 py-1.5 rounded-md text-xs border transition-all ${
              mode === "raw"
                ? "bg-[#1e3a5f] text-[#e8f0fe] border-[#2d7dd2]"
                : "bg-transparent text-[#7a8fa6] border-[#1e3a5f] hover:border-[#2d7dd2]"
            }`}
          >
            Raw
          </button>
        </div>
      </div>

      {/* Chart */}
      <ResponsiveContainer width="100%" height={380}>
        <RadarChart data={chartData} margin={{ top: 10, right: 30, bottom: 10, left: 30 }}>
          <PolarGrid stroke="#1e3a5f" />
          <PolarAngleAxis
            dataKey="stat"
            tick={{ fill: "#7a8fa6", fontSize: 11 }}
          />
          {mode === "percentile" && (
            <PolarRadiusAxis domain={[0, 100]} tick={false} axisLine={false} />
          )}
          <Radar
            name={result.player_name}
            dataKey="value"
            stroke="#2d7dd2"
            fill="#2d7dd2"
            fillOpacity={0.25}
            strokeWidth={2}
          />
          <Tooltip
            contentStyle={{
              background: "#080f1a",
              border: "1px solid #1e3a5f",
              borderRadius: "8px",
              fontSize: "0.8rem",
            }}
            formatter={(val: number) => [
              mode === "percentile" ? `${val}th percentile` : val,
              "Value",
            ]}
          />
        </RadarChart>
      </ResponsiveContainer>
    </div>
  )
}