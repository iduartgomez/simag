"""Helper functions and tools."""

import xml.etree.ElementTree as xml
from ast import literal_eval


def import_configs(cfg_file):

    def iter_balsheet(node):
        entries = {}
        for item in node:
            try:
                val = item.attrib['val']
            except:
                val = None
            quant = item.attrib['quant']
            entries[item.tag] = (quant, val)
        return entries

    data = xml.parse(cfg_file)
    root = data.getroot()
    cfg_type = root.attrib['cfg']
    if cfg_type == 'agents':
        agents = {}
        configs = {}
        for agent in root:
            ag_type = agent.attrib['type']
            ag_id = agent.attrib['id']
            if ag_type == 'instance':
                agents[ag_id] = {}
                for node in agent:
                    if node.tag == 'properties':
                        agents[ag_id]['properties'] = node.attrib
                    elif node.tag == 'position':
                        agents[ag_id]['position'] = literal_eval(node.text)
                    elif node.tag == 'assets':
                        agents[ag_id]['assets'] = iter_balsheet(node)
                    elif node.tag == 'liabilities':
                        agents[ag_id]['liabilities'] = iter_balsheet(node)
                    elif node.tag == 'percept_modes':
                        modes = node.text.split(',')
                        modes = [x.strip() for x in modes]
                        agents[ag_id]['properties']['percept_modes'] = modes
                    elif node.tag == 'eval_modes':
                        modes = node.text.split(',')
                        modes = [x.strip() for x in modes]
                        agents[ag_id]['properties']['eval_modes'] = modes
            else:
                assets, liabilities = {}, {}
                for node in agent:
                    if node.tag == 'properties':
                        props = node.attrib
                    elif node.tag == 'assets':
                        assets = iter_balsheet(node)
                    elif node.tag == 'liabilities':
                        liabilities = iter_balsheet(node)
                configs[ag_id] = [props, assets, liabilities]
        return agents, configs
