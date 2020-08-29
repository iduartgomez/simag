use futures_codec::{BytesMut, Decoder, Encoder};
use libp2p::{core::PublicKey, identity::Keypair};
use serde::{Deserialize, Serialize};
use std::io;
use unsigned_varint::codec::UviBytes;

pub(crate) const MAX_MSG_SIZE: usize = 4096;

#[derive(Serialize, Deserialize)]
pub(crate) struct Message {
    seqno: Vec<u8>,
    pub data: Vec<u8>,
    /// signature and from are only necessary if verification is required
    /// otherwise they will be empty non-allocated vectors
    signature: Vec<u8>,
    from: Vec<u8>,
}

impl Message {
    pub fn build<P>(payload: &P, keypair: &Keypair, verify: bool) -> Result<Message, ()>
    where
        P: Serialize + for<'a> Deserialize<'a>,
    {
        let data = bincode::serialize(payload).map_err(|_| ())?;
        let (from, signature) = {
            if verify {
                (
                    keypair.public().into_protobuf_encoding(),
                    keypair.sign(&data).map_err(|_| ())?,
                )
            } else {
                (vec![], vec![])
            }
        };
        Ok(Message {
            seqno: rand::random::<u64>().to_be_bytes().to_vec(),
            data,
            signature,
            from,
        })
    }
}

pub(crate) struct MessageCodec {
    verification: bool,
}

impl MessageCodec {
    pub fn new(verification: bool) -> Self {
        MessageCodec { verification }
    }
}

impl Encoder for MessageCodec {
    type Item = Message;
    type Error = io::Error;

    fn encode(&mut self, item: Self::Item, dst: &mut BytesMut) -> Result<(), Self::Error> {
        let msg = bincode::serialize(&item).map_err(|_| io::ErrorKind::InvalidInput)?;

        let mut codec: UviBytes<io::Cursor<Vec<u8>>> = UviBytes::default();
        codec.set_max_len(MAX_MSG_SIZE);
        codec.encode(io::Cursor::new(msg), dst)
    }
}

impl Decoder for MessageCodec {
    type Item = Message;
    type Error = io::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        let mut codec: UviBytes<io::Cursor<Vec<u8>>> = UviBytes::default();
        codec.set_max_len(MAX_MSG_SIZE);
        let packet = match codec.decode(src)? {
            Some(p) => p,
            None => return Ok(None),
        };
        let msg: Message =
            bincode::deserialize(&packet[..]).map_err(|_| io::ErrorKind::InvalidInput)?;

        // validate that the seqno is of the right type
        if msg.seqno.len() != 8 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "sequence number has an incorrect size",
            ));
        }

        if self.verification {
            // verify message signatures
            let public_key = PublicKey::from_protobuf_encoding(&msg.from)
                .map_err(|_| io::ErrorKind::InvalidInput)?;
            if !public_key.verify(&msg.data, &msg.signature) {
                return Err(io::ErrorKind::InvalidData.into());
            }
        }

        Ok(Some(msg))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn encode_decode() {
        let original_data = "This is a message".to_owned();
        let keypair = Keypair::generate_ed25519();
        let msg = Message::build(&original_data, &keypair, true).unwrap();

        let mut codec = MessageCodec::new(true);
        let mut encoded = BytesMut::new();
        codec.encode(msg, &mut encoded).unwrap();
        assert!(!encoded.is_empty());

        let decoded = codec.decode(&mut encoded).unwrap().unwrap();
        let msg_data: String = bincode::deserialize(&decoded.data).unwrap();
        assert_eq!(msg_data, original_data);
    }
}
