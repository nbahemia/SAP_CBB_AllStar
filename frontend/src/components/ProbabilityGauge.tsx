interface ProbabilityGaugeProps {
  value: number
}

export default function ProbabilityGauge({ value }: ProbabilityGaugeProps) {
  const color = value >= 70 ? "#00e5a0" : value >= 40 ? "#f5c518" : "#ff4d6d"
  const circumference = 2 * Math.PI * 54
  const offset = circumference - (value / 100) * circumference

  return (
    <div className="flex flex-col items-center">
      <div className="relative w-[140px] h-[140px]">
        <svg
          width="140"
          height="140"
          className="-rotate-90"
        >
          <circle cx="70" cy="70" r="54" fill="none" stroke="#1e2a3a" strokeWidth="12" />
          <circle
            cx="70"
            cy="70"
            r="54"
            fill="none"
            stroke={color}
            strokeWidth="12"
            strokeDasharray={circumference}
            strokeDashoffset={offset}
            strokeLinecap="round"
            style={{ transition: "stroke-dashoffset 1s ease, stroke 0.5s ease" }}
          />
        </svg>

        {/* Center text — sits on top of svg */}
        <div className="absolute inset-0 flex flex-col items-center justify-center rotate-0">
          <span className="text-[2.2rem] font-black leading-none tracking-wider" style={{ color }}>
            {value}%
          </span>
          <span className="text-[0.6rem] text-[#7a8fa6] tracking-widest uppercase mt-1">
            All-Star Prob
          </span>
        </div>
      </div>
    </div>
  )
}