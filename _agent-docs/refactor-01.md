i want to be able to run workflow/01_{01,02{a|b}} for just one specific site, without rerunning the entire script for all sites.

refactor the workflows/01_{01,02a,02b}*.R scripts so that, instead of a loop in the main function body, the core logic takes a site name as a function and only processes that site. then, the script should take an optional argument that is a comma-separate list of sites to process. if not provided, the script loops over all the sites that it finds.

also, by default, the script should skip sites that have already been processed (based on the existence the relevant target output files or rows in aggregated output files), unless an --overwrite argument is passed, in which case the script should ignore existing data and write new outputs.

make as few changes as possible to the core site-specific processing logic; only change the high-level structure of the script.

describe your plan for making these changes before implementing. document your plan and implementation strategy in _agent-docs/refactor-01.html as you go.
