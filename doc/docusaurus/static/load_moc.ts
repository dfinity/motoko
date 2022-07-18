import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";

const MOC_VERSION = "0.6.29";

async function addPackage(name, repo, version, dir) {
  const meta_url = `https://data.jsdelivr.com/v1/package/gh/${repo}@${version}/flat`;
  const base_url = `https://cdn.jsdelivr.net/gh/${repo}@${version}`;
  const response = await fetch(meta_url);
  const json = await response.json();
  const promises = [];
  const fetchedFiles = [];
  for (const f of json.files) {
    if (f.name.startsWith(`/${dir}/`) && /\.mo$/.test(f.name)) {
      const promise = (async () => {
        const content = await (await fetch(base_url + f.name)).text();
        const stripped = name + f.name.slice(dir.length + 1);
        fetchedFiles.push(stripped);
        Motoko.saveFile(stripped, content);
      })();
      promises.push(promise);
    }
  }
  Promise.all(promises).then(() => {
    Motoko.addPackage(name, name + "/");
  });
}

function attachOnRunButton() {
  if (document.getElementsByClassName("run-button").length > 0) {
    const script = document.createElement("script");
    script.async = true;
    script.src = `/moc_interpreter.js`;

    script.addEventListener("load", () => {
      addPackage("base", "dfinity/motoko-base", `moc-${MOC_VERSION}`, "src");
      console.log(`moc ${MOC_VERSION} loaded`);
      // Run code
      const btns = document.getElementsByClassName("run-button run");
      for (var i = 0; i < btns.length; i++) {
        btns[i].click();
      }
    });
    document.head.appendChild(script);
  } else {
    console.log("motoko not loaded");
  }
}

if (
  ExecutionEnvironment.canUseDOM &&
  ExecutionEnvironment.canUseEventListeners
) {
  // As soon as the site loads in the browser, register a global event listener
  window.addEventListener("load", () => {
    attachOnRunButton();
  });
}

export function onRouteDidUpdate({ location, previousLocation }) {
  // Don't execute if we are still on the same page; the lifecycle may be fired
  // because the hash changes (e.g. when navigating between headings)
  if (location.pathname !== previousLocation?.pathname) {
    attachOnRunButton();
  }
}
