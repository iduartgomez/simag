use simag_core::Agent;

use crate::{
    agent::{AgentId, AgentPolicy},
    GroupSettings, NetworkHandle,
};

#[derive(Debug, serde::Serialize, serde::Deserialize)]
struct Settings;

#[typetag::serde]
impl GroupSettings for Settings {
    fn is_allowed_to_join(
        &self,
        _agent: AgentId,
        _petitioner_settings: &dyn GroupSettings,
    ) -> bool {
        true
    }

    fn box_cloned(&self) -> Box<dyn GroupSettings> {
        Box::new(Settings)
    }
}

fn set_listener(mut network: NetworkHandle<String>) {
    let agent = Agent::new("agent_01".to_owned());
    let op_id = network.register_agent(&agent, AgentPolicy::default());
    let listening_ag_key = network.get_resource_key(op_id, None).unwrap();
    // network.create_group("group_01", &["agent_01"], None, Settings {});

    let mut served = false;
    while network.running() {
        if let Some(stats) = network.stats.for_key(&listening_ag_key) {
            if stats.times_served > 0 && !served {
                println!("Served a resource at least once");
                served = true;
            }
        }
    }
}

fn set_dialer(mut network: NetworkHandle<String>) {
    let agent = Agent::new("agent_02".to_owned());
    let op_id = network.register_agent(&agent, AgentPolicy::default());
    let this_ag_key = network.get_resource_key(op_id, None).unwrap();

    let query = network.find_agent("agent_01");
    let listening_ag_key = network.get_resource_key(query, None).unwrap();

    let mut served = false;
    while network.running() {
        if let Some(stats) = network.stats.for_key(&listening_ag_key) {
            if stats.times_received > 0 && !served {
                println!("Received a resource at least once");
                served = true;
            }
        }
    }
}

#[test]
fn connect_agents() {
    let port = super::get_free_port().unwrap();

    let listener = std::thread::spawn(move || {
        let network = super::config_listener(port);
        set_listener(network);
    });

    let dialer = std::thread::spawn(move || {
        let (network, _) = super::config_dialer(port);
        set_dialer(network);
    });

    assert!(dialer.join().is_ok());
    assert!(listener.join().is_ok());
}
