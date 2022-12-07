const fs = require("fs");
const path = require("path");

const inputText = () =>
  fs.readFileSync(path.join(__dirname, "./sample-input.txt")).toString();
/**
 * @param {string} line
 * @return {Array<{ id: number; value: string | null}>}
 */
function parseLine(line) {
  const matches = line.matchAll(/(\[\w\] ?|   )/g);

  return (
    [...matches].map((match, i) => {
      if (!match) throw Error("Unmatched");
      if (match[0].trim() == "") {
        return { id: i + 1, value: null };
      } else {
        return { id: i + 1, value: match[0].trim() };
      }
    }) || []
  );
}

/**
 * @param {Map<number, string[]>} crates
 */
function printCrates(crates) {
  for (const [key, value] in crates.entries()) {
    console.log(key, String.Join(" ", value));
  }

  console.log();
}

/**
 * @param {string} input
 * @returns {Map<number, string[]>}
 */
function parseCrates(input) {
  /**@type {Map<number, string[]>} */
  const initialMap = new Map();
  return input
    .split("\r\n")
    .reverse()
    .slice(1)
    .reduce(
      (crates, line) =>
        parseLine(line).reduce((crates, { id, value: contents }) => {
          /**@type {string[]} */
          let crate = [];
          if (!crates.has(id)) {
            crates.set(id, crate);
          } else {
            crate = crates.get(id);
          }
          if (contents) {
            crate.push(contents);
          }
          return crates;
        }, crates),
      initialMap
    );
}

/**
 * @param {string} input
 */
function parseInstructions(input) {
  let reg = /move (\d+) from (\d+) to (\d+)/;

  return input.split("\r\n").map((line) => {
    let match = line.match(reg);

    if (!match) throw Error("Invalid inpupt");

    let [, count, from, to] = match;

    return {
      From: parseInt(from),
      To: parseInt(to),
      Count: parseInt(count),
    };
  });
}

function parseSegments() {
  const [crates, instructions] = inputText().split("\r\n\r\n");

  return {
    crateMap: parseCrates(crates),
    instructions: parseInstructions(instructions),
  };
}

function partOne() {
  let i = 1;
  const { crateMap, instructions } = parseSegments();

  printCrates(crateMap);

  for (command of instructions) {
    let fromCrate = crateMap.get(command.From);
    let toCrate = crateMap.get(command.To);

    console.log(
      `Step ${i}: move ${command.Count} from ${command.From} to ${command.To}`
    );

    for (let j = 1; j < command.Count; j++) {
      let item = fromCrate.pop();
      toCrate.push(item);

      //printCrates crateMap
      i++;
    }
  }

  printCrates(crateMap);

  let output = "";
  for (const [key, crate] of crateMap) {
    const value = crate[crate.length -1];
    if (value) {
      output += value.replace("[", "").replace("]", "");
    }
  }

  console.log("Result", output);
}
partOne();
