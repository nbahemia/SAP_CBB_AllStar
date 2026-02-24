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

type ViewMode = "percentile" | "raw" | "stats"

const PCT_STATS = ["eFG", "TS_per", "ORB_per", "DRB_per", "AST_per", "TO_per", "blk_per", "stl_per", "usg"]

function formatRaw(key: string, value: number): string {
  if (PCT_STATS.includes(key)) return `${Math.round(value * 100) / 100}%`
  return `${Math.round(value * 100) / 100}`
}

function getOrdinalSuffix(value: number): string {
  if (value % 100 >= 11 && value % 100 <= 13) return "th"
  switch (value % 10) {
    case 1:
      return "st"
    case 2:
      return "nd"
    case 3:
      return "rd"
    default:
      return "th"
  }
}

function CustomTick({ x, y, payload, chartData, mode }: any) {
  const index = chartData.findIndex((d: any) => d.stat === payload.value)
  const d = chartData[index]
  const rawKey = Object.keys(RAW_LABELS)[index]

  const valueLabel = d == null ? "" : mode === "percentile"
    ? `${d.value}${getOrdinalSuffix(d.value)}`
    : formatRaw(rawKey, d.value)

  return (
    <text x={x} y={y} textAnchor="middle" dominantBaseline="central">
      <tspan x={x} dy="-0.5em" fontSize={10} fill="#7a8fa6">
        {payload.value}
      </tspan>
      <tspan x={x} dy="1.4em" fontSize={11} fontWeight="700" fill="#e8f0fe">
        {valueLabel}
      </tspan>
    </text>
  )
}

export default function StatsRadar({ result }: StatsRadarProps) {
  const [mode, setMode] = useState<ViewMode>("percentile")

  const chartData =
    mode === "raw"
      ? Object.entries(RAW_LABELS).map(([key, label]) => ({
        stat: label,
        value: result.raw_stats?.[key] != null ? Math.round(result.raw_stats[key] * 100) / 100 : 0,
        rawKey: key,
      }))
      : Object.entries(PERCENTILE_LABELS).map(([key, label]) => ({
        stat: label,
        value: result.percentile_stats?.[key] != null ? Math.round(result.percentile_stats[key] * 100) / 100 : 0,
        fullMark: 100,
        rawKey: key,
      }))

  const tabs: { key: ViewMode; label: string }[] = [
    { key: "percentile", label: "Percentile" },
    { key: "raw", label: "Raw" },
    { key: "stats", label: "Stats" },
  ]

  return (
    <div className="bg-[#0f1e30] border border-[#1e3a5f] rounded-2xl p-6">
      <div className="flex justify-between items-center mb-5">
        <span className="text-xs text-[#7a8fa6] tracking-[3px] uppercase">Player Profile</span>
        <div className="flex gap-2">
          {tabs.map((t) => (
            <button
              key={t.key}
              onClick={() => setMode(t.key)}
              className={`px-4 py-1.5 rounded-md text-xs border transition-all ${mode === t.key
                ? "bg-[#1e3a5f] text-[#e8f0fe] border-[#2d7dd2]"
                : "bg-transparent text-[#7a8fa6] border-[#1e3a5f] hover:border-[#2d7dd2]"
                }`}
            >
              {t.label}
            </button>
          ))}
        </div>
      </div>

      {mode !== "stats" && (
        <ResponsiveContainer width="100%" height={360}>
          <RadarChart
            data={chartData}
            margin={{ top: 50, right: 50, bottom: 30, left: 50 }}
            cx="50%"
            cy="50%"
            outerRadius="95%"
          >
            <PolarGrid stroke="#1e3a5f" />
            <PolarAngleAxis
              dataKey="stat"
              tickSize={25}
              tick={(props) => <CustomTick {...props} chartData={chartData} mode={mode} />}
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
              formatter={(val: number, _: any, props: any) => {
                const key = props?.payload?.rawKey ?? ""
                if (mode === "percentile") return [`${val}${getOrdinalSuffix(val)} percentile`, props.payload.stat]
                return [formatRaw(key, val), props.payload.stat]
              }}
            />
          </RadarChart>
        </ResponsiveContainer>
      )}

      {mode === "stats" && (
        <div className="grid grid-cols-3 gap-2.5">
          {Object.entries(RAW_LABELS).map(([key, label]) => {
            const percentileKey = `${key}_percentile`
            const raw = result.raw_stats?.[key]
            const percentile = result.percentile_stats?.[percentileKey]
            return (
              <div key={key} className="bg-[#080f1a] border border-[#1e3a5f] rounded-lg px-3 py-2">
                <p className="text-[0.7rem] text-[#7a8fa6] mb-1">{label}</p>
                <p className="text-sm font-bold">
                  {raw != null ? formatRaw(key, raw) : "—"}
                </p>
                {percentile != null && (
                  <p className="text-[0.65rem] text-[#2d7dd2] mt-0.5">
                    {Math.round(percentile)}{getOrdinalSuffix(Math.round(percentile))} percentile
                  </p>
                )}
              </div>
            )
          })}
        </div>
      )}
    </div>
  )
}
