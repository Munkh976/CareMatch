import pandas as pd
import numpy as np
from faker import Faker
from geopy.distance import geodesic
from datetime import datetime, timedelta
import random
import os

class DataGenerator:
    def __init__(self):
        self.data = {
            'elderly': pd.DataFrame(),
            'volunteers': pd.DataFrame(),
            'care_requests': pd.DataFrame(),
            'elderly_requests': pd.DataFrame(),
            'volunteer_responses': pd.DataFrame(),
            'availability': pd.DataFrame(),
            'needs': pd.DataFrame(),
            'skills': pd.DataFrame(),
            'elderly_needs': pd.DataFrame(),
            'volunteer_skills': pd.DataFrame()
        }
        self.fake = Faker()
        np.random.seed(42)
        random.seed(42)
        self.TIME_SLOTS = ["Weekday AM", "Weekday PM", "Weekend AM", "Weekend PM"]
        self.MATCHABLE_SKILLS = ["Driving", "Cooking", "Cleaning", "Companionship",
                                 "Shopping", "Medication", "Transportation", "First Aid"]
        self.URGENCIES = ["Low", "Medium", "High"]
        self.ZIPCODES = [
            {"zipcode": "10001", "major_city": "New York", "state": "NY", "lat": 40.7508, "lng": -73.9961},
            {"zipcode": "10002", "major_city": "New York", "state": "NY", "lat": 40.7178, "lng": -73.9909},
            {"zipcode": "10003", "major_city": "New York", "state": "NY", "lat": 40.7322, "lng": -73.9881},
            {"zipcode": "10004", "major_city": "New York", "state": "NY", "lat": 40.7031, "lng": -74.0133}
        ]

    def generate_needs(self):
        data = [{"need_id": i+1, "need_name": skill} for i, skill in enumerate(self.MATCHABLE_SKILLS)]
        self.data['needs'] = pd.DataFrame(data)

    def generate_skills(self):
        data = [{"skill_id": i+1, "skill_name": skill} for i, skill in enumerate(self.MATCHABLE_SKILLS)]
        self.data['skills'] = pd.DataFrame(data)

    def generate_elderly(self, count=100):
        data = []
        for i in range(1, count+1):
            zip_data = random.choice(self.ZIPCODES)
            data.append({
                'id': f"E{i:03d}",
                'name': self.fake.name(),
                'zipcode': zip_data["zipcode"],
                'location': zip_data["zipcode"],
                'contact_number': f"+1{self.fake.msisdn()[3:]}",
                'age': random.randint(65, 95),
                'preferred_time_slots': ', '.join(random.sample(self.TIME_SLOTS, 2)),
                'major_city': zip_data["major_city"],
                'state': zip_data["state"],
                'lat': zip_data["lat"] + random.uniform(-0.01, 0.01),
                'lng': zip_data["lng"] + random.uniform(-0.01, 0.01)
            })
        self.data['elderly'] = pd.DataFrame(data)

    def generate_volunteers(self, count=50):
        data = []
        for i in range(1, count+1):
            zip_data = random.choice(self.ZIPCODES)
            num_skills = random.randint(3, 6)
            skills = random.sample(self.MATCHABLE_SKILLS, num_skills)
            data.append({
                'id': f"V{i:03d}",
                'name': self.fake.name(),
                'zipcode': zip_data["zipcode"],
                'location': zip_data["zipcode"],
                'contact_number': f"+1{self.fake.msisdn()[3:]}",
                'availability': ', '.join(random.sample(self.TIME_SLOTS, 2)),
                'radius_km': random.choice([5, 10, 15]),
                'radius_willingness': random.choice([5, 10, 15]),
                'major_city': zip_data["major_city"],
                'state': zip_data["state"],
                'lat': zip_data["lat"] + random.uniform(-0.01, 0.01),
                'lng': zip_data["lng"] + random.uniform(-0.01, 0.01),
                'response_rate': round(np.random.beta(2, 3) + 0.2, 2),
                'skills': ', '.join(skills)
            })
        self.data['volunteers'] = pd.DataFrame(data)

    def generate_elderly_needs(self):
        data = []
        for elder in self.data['elderly'].itertuples():
            num_needs = random.randint(2, 4)
            need_ids = random.sample(list(self.data['needs']['need_id']), num_needs)
            for need_id in need_ids:
                data.append({
                    'elderly_id': elder.id,
                    'need_id': need_id
                })
        self.data['elderly_needs'] = pd.DataFrame(data)

    def generate_volunteer_skills(self):
        data = []
        for vol in self.data['volunteers'].itertuples():
            skills = vol.skills.split(', ')
            for skill in skills:
                skill_id = self.data['skills'].loc[self.data['skills']['skill_name'] == skill, 'skill_id'].iloc[0]
                data.append({
                    'volunteer_id': vol.id,
                    'skill_id': skill_id
                })
        self.data['volunteer_skills'] = pd.DataFrame(data)

    def generate_care_requests(self):
        data = []
        request_id = 1
        for elder in self.data['elderly'].itertuples():
            for _ in range(random.randint(2, 5)):
                elder_needs = self.data['elderly_needs'][self.data['elderly_needs']['elderly_id'] == elder.id]
                need_ids = elder_needs['need_id'].tolist()
                needs = [self.data['needs'].loc[self.data['needs']['need_id'] == nid, 'need_name'].iloc[0] for nid in need_ids]
                data.append({
                    'id': f"CR{request_id:03d}",
                    'elderly_id': elder.id,
                    'needs': ', '.join(needs),
                    'time_prefs': ', '.join(random.sample(self.TIME_SLOTS, 2)),
                    'urgency': random.choice(self.URGENCIES),
                    'created_at': self.fake.date_time_between(start_date='-6m', end_date='now')
                })
                request_id += 1
        self.data['care_requests'] = pd.DataFrame(data)

    def generate_elderly_requests(self):
        data = []
        request_id = 1
        status_distribution = {'pending': 0.3, 'completed': 0.7}
        for req in self.data['care_requests'].itertuples():
            status = random.choices(
                list(status_distribution.keys()),
                weights=list(status_distribution.values())
            )[0]
            requested_vols = ""
            if random.random() < 0.3:
                requested_vols = ','.join(random.sample(self.data['volunteers']['id'].tolist(), 2))
            data.append({
                'id': f"ER{request_id:03d}",
                'request_id': req.id,
                'elderly_id': req.elderly_id,
                'requested_volunteers': requested_vols,
                'status': status,
                'modified_at': req.created_at + timedelta(hours=random.randint(1, 72))
            })
            request_id += 1
        self.data['elderly_requests'] = pd.DataFrame(data)

    def generate_volunteer_responses(self):
        data = []
        response_id = 1
        completed_requests = self.data['elderly_requests'][self.data['elderly_requests']['status'] == 'completed']
        for req in completed_requests.itertuples():
            potential_volunteers = (
                req.requested_volunteers.split(',')
                if req.requested_volunteers
                else random.sample(self.data['volunteers']['id'].tolist(), 2))
            for vol_id in potential_volunteers:
                vol_response_rate = self.data['volunteers'].loc[
                    self.data['volunteers']['id'] == vol_id, 'response_rate'].values[0]
                acceptance_prob = min(0.9, vol_response_rate * 1.2)
                status = np.random.choice(
                    ['accepted', 'rejected'],
                    p=[acceptance_prob, 1-acceptance_prob]
                )
                data.append({
                    'id': f"VR{response_id:03d}",
                    'request_id': req.request_id,
                    'volunteer_id': vol_id,
                    'status': status,
                    'responded_at': req.modified_at + timedelta(hours=random.randint(1, 24))
                })
                response_id += 1
        self.data['volunteer_responses'] = pd.DataFrame(data)

    def generate_availability(self):
        data = []
        for vol in self.data['volunteers'].itertuples():
            slots = random.sample(self.TIME_SLOTS, random.randint(2, 3))
            if random.random() < 0.3:
                if "Weekend AM" in slots:
                    slots.append("Weekend PM")
                elif "Weekday AM" in slots:
                    slots.append("Weekday PM")
            for slot in slots:
                data.append({
                    'volunteer_id': vol.id,
                    'time_slot': slot,
                    'reliability': round(random.uniform(0.7, 1.0), 2)
                })
        self.data['availability'] = pd.DataFrame(data)

    def save_to_csv(self):
        os.makedirs('data', exist_ok=True)
        for name, df in self.data.items():
            for col in df.columns:
                if pd.api.types.is_datetime64_any_dtype(df[col]):
                    df[col] = df[col].dt.strftime('%Y-%m-%d %H:%M:%S')
            df.to_csv(f'data/{name}.csv', index=False)
            print(f"✅ Generated data/{name}.csv with {len(df)} records")

    def generate_all(self):
        print("Generating synthetic data...")
        self.generate_needs()
        self.generate_skills()
        self.generate_elderly()
        self.generate_volunteers()
        self.generate_elderly_needs()
        self.generate_volunteer_skills()
        self.generate_care_requests()
        self.generate_elderly_requests()
        self.generate_volunteer_responses()
        self.generate_availability()
        self.save_to_csv()
        print("\n📊 Data Summary:")
        print(f"- Elderly: {len(self.data['elderly'])}")
        print(f"- Volunteers: {len(self.data['volunteers'])}")
        print(f"- Care Requests: {len(self.data['care_requests'])}")
        print("\n✅ Data generation complete!")

if __name__ == "__main__":
    generator = DataGenerator()
    generator.generate_all()