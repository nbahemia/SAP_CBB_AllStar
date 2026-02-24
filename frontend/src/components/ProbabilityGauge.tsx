interface ProbabilityGaugeProps {
  value: number
}

export default function ProbabilityGauge({ value }: ProbabilityGaugeProps) {
  const color = value >= 70 ? "#00e5a0" : value >= 40 ? "#f5c518" : "#ff4d6d"
  const circumference = 2 * Math.PI * 38
  const offset = circumference - (value / 100) * circumference

  return (
    <div className="flex flex-col items-center">
      <div className="relative w-[100px] h-[100px]">
        <svg width="100" height="100" className="-rotate-90">
          <circle cx="50" cy="50" r="38" fill="none" stroke="#1e2a3a" strokeWidth="10" />
          <circle cx="50" cy="50" r="38" fill="none" stroke={color}
            strokeWidth="10" strokeDasharray={circumference} strokeDashoffset={offset}
            strokeLinecap="round"
            style={{ transition: "stroke-dashoffset 1s ease, stroke 0.5s ease" }}
          />
        </svg>
        <div className="absolute inset-0 flex flex-col items-center justify-center">
          <span className="text-lg font-black leading-none tracking-wider" style={{ color }}>
            {value}%
          </span>
          <span className="text-[0.55rem] text-[#7a8fa6] tracking-widest uppercase mt-0.5">
            All-Star
          </span>
        </div>
      </div>
    </div>
  )
}