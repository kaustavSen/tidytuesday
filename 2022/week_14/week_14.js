const drawChart = async () => {
  const data = await d3.csv("plot_data.csv", d3.autoType)

  let chartDimensions = {
    width: 900,
    height: 900 * 0.6,
    margin: {
      top: 20,
      right: 60,
      bottom: 20,
      left: 60
    }
  }
  chartDimensions.boundedWidth = chartDimensions.width
    - chartDimensions.margin.left
    - chartDimensions.margin.right

  chartDimensions.boundedHeight = chartDimensions.height
    - chartDimensions.margin.top
    - chartDimensions.margin.bottom

  const xAccessor = d => d.year_founded
  const yAccessor = d => d.n

  const xScale = d3.scaleLinear()
    .domain(d3.extent(data, xAccessor))
    .range([0, chartDimensions.boundedWidth])
    .nice()

  const yScale = d3.scaleLinear()
    .domain([0, d3.max(data, yAccessor)])
    .range([chartDimensions.boundedHeight, 0])

  const lineGenerator = d3.line()
    .x(d => xScale(xAccessor(d)))
    .y(d => yScale(yAccessor(d)))

  const xAxis = d3.axisBottom()
    .ticks(6)
    .tickFormat(d3.format(""))
    .scale(xScale)

  const yAxis = d3.axisLeft()
    .tickValues([5, 25, 45, 65])
    .tickFormat(d3.format(""))
    .tickPadding(2)
    .tickSize(-(chartDimensions.boundedWidth + chartDimensions.margin.left * 0.2))
    .scale(yScale)

  const svg = d3.select(".chart")
    .append("svg")
      .attr("width", chartDimensions.width)
      .attr("height", chartDimensions.height)

  const bounds = svg.append("g")
      .attr("transform", `translate(${chartDimensions.margin.left}, ${chartDimensions.margin.top})`)

  bounds.append("g")
      .attr("class", "axis")
      .style("transform", `translateY(${chartDimensions.boundedHeight}px)`)
    .call(xAxis)
    .call(g => g.select(".domain").remove())
    .call(g => g.selectAll(".tick line").remove())
  
  bounds.append("g")
      .attr("class", "axis")
    .call(yAxis)
    .call(g => g.select(".domain").remove())
    .call(g => g.selectAll(".tick line")
        .attr("stroke-opacity", 0.2)
        .attr("stroke-width", 1.1))
    .call(g => g.selectAll(".tick text")
        .attr("x", 15)
        .attr("dy", -5))

  const line = bounds.append("path")
      .attr("d", lineGenerator(data))
      .attr("fill", "none")
      .attr("stroke", "#676767")
      .attr("stroke-width", 2.5)

  bounds.append("rect")
      .attr("x", xScale(2000))
      .attr("height", chartDimensions.boundedHeight)
      .attr("width", xScale(2014) - xScale(2000))
      .style("fill", "#d8dbdc")
      .style("opacity", 0.3)

  const annotations = [{
    note: {
      title: "Think digital act local",
      bgPadding: 0,
      wrap: 320
    },
    id: "annotation-title",
    dx: 48,
    dy: 136,
    disable: ["connector", "subject"]
  },
  {
    note: {
      label: "The number of new local news organisations setup each year",
      bgPadding: 0,
      wrap: 280,
    },
    id: "annotation-sub-title",
    dx: 48,
    dy: 230,
    disable: ["connector", "subject"]
  },
  {
    note: {
      label: "Up until the end of the 1990s the presence of local news agencies was next to none",
      bgPadding: 0,
      wrap: 400
    },
    id: "annotation-text-1",
    dx: 43,
    dy: 444,
    disable: ["connector", "subject"]
  },
  {
    note: {
      label: "The rise of the dot-com era from the 2000s catapulted local new organizations towards a trajectory of exponential growth",
      bgPadding: 0,
      wrap: 250
    },
    id: "annotation-text-2",
    dx: 521,
    dy: 23,
    disable: ["connector", "subject"]
  },
  {
    note: {
      label: "However, the last few years have seen a decline in the number of new local enterprises coming up indicating that the sector might be inching towards a saturation point",
      bgPadding: 0,
      wrap: 250
    },
    id: "annotation-text-3",
    dx: 735,
    dy: 375,
    disable: ["connector", "subject"]
  },
  {
    note: {
      label: "Data: Project Oasis | Plot: Kaustav Sen",
      bgPadding: 0,
      wrap: 250
    },
    id: "annotation-caption",
    dx: 591,
    dy: 529,
    disable: ["connector", "subject"]
  }]

  const makeAnnotations = d3.annotation()
    .editMode(false)
    .annotations(annotations)

  document.fonts.ready.then(function() {
    bounds.append("g")
        .attr("class", "annotations-group")
      .call(makeAnnotations)
      .call(g => g.selectAll(".note-line").remove())

    bounds.selectAll(".annotation")
      .data(annotations)
      .join("g")
        .attr("id", d => d.id)

    const cssSelector = "#annotation-title > g.annotation-note > g > text.annotation-note-title > tspan:nth-child(2)"
    bounds.select(cssSelector)
      .attr("dy", "0.95em")
  })

  const button = d3.select("#get-annotations")

  button.on("click", () => {
    const currPosition = makeAnnotations.collection().annotations
    console.log(currPosition)
  })



  // console.log(makeAnnotations.collection().annotations)

}
drawChart()