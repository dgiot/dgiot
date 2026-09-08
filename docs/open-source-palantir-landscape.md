# The Open-Source Palantir Landscape — a Reality-Checked Survey

> Companion to [comparison.md](comparison.md) ("the brain and the limbs").
> Every project below was verified against GitHub in September 2026; claims that
> could not be verified are listed as such instead of being silently repeated.
> Star counts are approximate and move daily.

## Verified projects

| Project | GitHub | Stars | Stack | What it is |
|---|---|---|---|---|
| **WorldMonitor** | [koala73/worldmonitor](https://github.com/koala73/worldmonitor) | ~85.8K | TypeScript | Real-time global intelligence dashboard: AI-powered news aggregation, geopolitical monitoring, infrastructure tracking |
| **Semantica** | [semantica-agi/semantica](https://github.com/semantica-agi/semantica) | ~12.4K | Python | Graph-native infrastructure for context and **accountable AI systems** |
| **OpenFoundry** | [u485349-coder/OpenFoundry](https://github.com/u485349-coder/OpenFoundry) | ~109 | Rust + Svelte | Explicit "open-source Palantir Foundry alternative": connect data sources → build ontologies → pipelines → dashboards → AI decisions. Self-hosted |
| OpenFoundry (OSDK-compatible) | [Przyval/openfoundry](https://github.com/Przyval/openfoundry) | ~22 | TypeScript | Ontology-first data platform, 100% Foundry OS SDK compatible |
| OpenFoundry (independent) | [Shadowfax-Data/OpenFoundry](https://github.com/Shadowfax-Data/OpenFoundry) | ~29 | TypeScript | Same name, independent implementation |
| nano-ontoprompt | [sdzsxjl/nano-ontoprompt](https://github.com/sdzsxjl/nano-ontoprompt) | 1 | Python | Micro ontology platform inspired by Foundry: visual pipeline mapping cleaned data to entities, relations and actions |
| OSIRIS (deployment only) | [AlfredoSuarez/osiris-netlify](https://github.com/AlfredoSuarez/osiris-netlify) | 0 | TypeScript | Netlify-ready deployment of an OSINT intelligence dashboard; the canonical upstream repo could not be located on GitHub |

## Claims circulating in secondary sources that we could NOT verify

- **"AEGIS"** — described as a Gotham-inspired knowledge-graph / OWL-ontology platform for critical-infrastructure protection. No matching GitHub repository found under that description as of 2026-09.
- **"Akashic"** — described as a self-hosted intelligence workspace mapping aircraft/satellite/earthquake data onto an interactive map. No matching repository found.
- **OpenFoundry "written in Go with 41 microservices"** — the prominent implementation is Rust + Svelte; no Go/41-microservice variant was found. Treat this claim as unconfirmed.
- **Osiris** as a standalone "open-source global intelligence platform" — only a third-party deployment wrapper was found; the original project remains unlocated.

## What the verified landscape means for DGIOT

1. **WorldMonitor (~85.8K★) proves the demand side.** Heterogeneous real-time feeds →
   one situational dashboard is a mass-market pattern. DGIOT plays the same pattern
   with industrial devices instead of global news: `DeviceAccess → UnifiedPipeline →
   StreamEngine → Alert` (DLAS EDGE layer).
2. **Semantica (~12.4K★) validates ontology + accountability for AI agents.** That is
   exactly the DGIOT LOGIC layer's trajectory: Ontology Engine + reasoning, with the
   audit trail ontology provides. Watch it; it is the strongest "brain-side" OSS.
3. **OpenFoundry shows the "Foundry-shaped" product surface** (datasets → ontology →
   actions → dashboards) but has tiny traction (~109★). Useful as an architecture
   mirror, not as a moat.
4. **nano-ontoprompt is a UX sketch, not a platform** (1★). Reference it only for
   minimal ontology-mapping interaction design.
5. **Nobody on this list owns the industrial device layer** — 300+ protocols, edge
   gateways, thing models, OT time-series at scale. That is DGIOT's lane, and it is
   complementary to every "brain" project above (see comparison.md).

**Bottom line:** the verified landscape confirms the ontology-first direction DGIOT
already ships, and confirms the open periphery: the "brain" projects are crowding
fast, the "limbs and senses" layer remains open. Ship the protocol depth.
